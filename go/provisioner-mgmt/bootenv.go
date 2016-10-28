package main

import (
	"bytes"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"net/url"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"strings"
	"text/template"
	"time"

	"github.com/digitalrebar/digitalrebar/go/rebar-api/api"
)

// RenderData is the struct that is passed to templates as a source of
// parameters and useful methods.
type RenderData struct {
	Machine        *Machine // The Machine that the template is being rendered for.
	Env            *BootEnv // The boot environment that provided the template.
	ProvisionerURL string   // The URL to the provisioner that all files should be fetched from
	CommandURL     string   // The URL of the API endpoint that this machine should talk to for command and control
	TenantId       int      // The Tenant that this BootEnv belongs in
}

func (r *RenderData) ProvisionerAddress() string {
	return ourAddress
}

// BootParams is a helper function that expands the BootParams
// template from the boot environment.
func (r *RenderData) BootParams() (string, error) {
	res := &bytes.Buffer{}
	if r.Env.bootParamsTmpl == nil {
		return "", nil
	}
	if err := r.Env.bootParamsTmpl.Execute(res, r); err != nil {
		return "", err
	}
	return res.String(), nil
}

func (r *RenderData) ParseUrl(segment, rawUrl string) (string, error) {
	parsedUrl, err := url.Parse(rawUrl)
	if err != nil {
		return "", err
	}
	switch segment {
	case "scheme":
		return parsedUrl.Scheme, nil
	case "host":
		return parsedUrl.Host, nil
	case "path":
		return parsedUrl.Path, nil
	}
	return "", fmt.Errorf("No idea how to get URL part %s from %s", segment, rawUrl)
}

// Param is a helper function for extracting a parameter from Machine.Params
func (r *RenderData) Param(key string) (interface{}, error) {
	res, ok := r.Machine.Params[key]
	if !ok {
		return nil, fmt.Errorf("No such machine parameter %s", key)
	}
	return res, nil
}

// TemplateInfo holds information on the templates in the boot
// environment that will be expanded into files.
type TemplateInfo struct {
	Name string // Name of the template
	Path string // A template that specifies how to create
	// the final path the template should be
	// written to.
	UUID      string // The UUID of the template that should be expanded.
	pathTmpl  *template.Template
	finalPath string
	contents  *Template
}

type FileData struct {
	URL              string // The URL to get the file
	Name             string // Name of file in the install directory
	ValidationURL    string // The URL to get a checksum or signature file
	ValidationMethod string // The method to validate the file.
}

// OsInfo holds information about the operating system this BootEnv maps to.
// Most of this information is optional for now.
type OsInfo struct {
	Name      string      // The name of the OS this BootEnv has.  Required.
	Family    string      // The family of operating system (linux distro lineage, etc)
	Codename  string      // The codename of the OS, if any.
	Version   string      // The version of the OS, if any.
	IsoFile   string      // The name of the ISO that the OS should install from.
	IsoSha256 string      // The SHA256 of the ISO file.  Used to check for corrupt downloads.
	IsoUrl    string      // The URL that the ISO can be downloaded from, if any.
	Files     []*FileData // A list of files to download along with an ISO.
}

func (o *OsInfo) InstallUrl() string {
	return provisionerURL + "/" + path.Join(o.Name, "install")
}

// BootEnv encapsulates the machine-agnostic information needed by the
// provisioner to set up a boot environment.
type BootEnv struct {
	Name           string          // The name of the boot environment.
	OS             *OsInfo         // The OS specific information for the boot environment.
	Templates      []*TemplateInfo // The templates that should be expanded into files for the bot environment.
	Kernel         string          // The partial path to the kernel in the boot environment.
	Initrds        []string        // Partial paths to the initrds that should be loaded for the boot environment.
	BootParams     string          // A template that will be expanded to create the full list of boot parameters for the environment.
	RequiredParams []string        // The list of extra required parameters for this bootstate. They should be present as Machine.Params when the bootenv is applied to the machine.
	Available      bool
	bootParamsTmpl *template.Template
	TenantId       int
	Errors         []string
}

func (b *BootEnv) Error() string {
	return strings.Join(b.Errors, "\n")
}

func (b *BootEnv) errorOrNil() error {
	if len(b.Errors) == 0 {
		return nil
	}
	return b
}

func (b *BootEnv) Errorf(arg string, args ...interface{}) {
	b.Errors = append(b.Errors, fmt.Sprintf(arg, args...))
}

// PathFor expands the partial paths for kernels and initrds into full
// paths appropriate for specific protocols.
//
// proto can be one of 3 choices:
//    http: Will expand to the URL the file can be accessed over.
//    tftp: Will expand to the path the file can be accessed at via TFTP.
//    disk: Will expand to the path of the file inside the provisioner container.
func (b *BootEnv) PathFor(proto, f string) string {
	res := b.OS.Name
	if res != "discovery" {
		res = path.Join(res, "install")
	}
	switch proto {
	case "disk":
		return path.Join(fileRoot, res, f)
	case "tftp":
		return path.Join(res, f)
	case "http":
		return provisionerURL + "/" + path.Join(res, f)
	default:
		logger.Fatalf("Unknown protocol %v", proto)
	}
	return ""
}

func (b *BootEnv) parseTemplates() {
	for _, templateParams := range b.Templates {
		pathTmpl, err := template.New(templateParams.Name).Parse(templateParams.Path)
		if err != nil {
			b.Errorf("bootenv: Error compiling path template %s (%s): %v",
				templateParams.Name,
				templateParams.Path,
				err)
			continue
		}
		templateParams.pathTmpl = pathTmpl.Option("missingkey=error")
		if templateParams.contents == nil {
			tmpl := &Template{UUID: templateParams.UUID}
			if err := backend.load(tmpl); err != nil {
				b.Errorf("bootenv: Error loading template %s for %s: %v",
					templateParams.UUID,
					templateParams.Name,
					err)
				continue
			}
			if err := tmpl.Parse(); err != nil {
				b.Errorf("bootenv: Error compiling template %s: %v\n---template---\n %s",
					templateParams.Name,
					err,
					tmpl.Contents)
				continue
			}
			templateParams.contents = tmpl
		}

	}
	if b.BootParams != "" {
		tmpl, err := template.New("machine").Parse(b.BootParams)
		if err != nil {
			b.Errorf("bootenv: Error compiling boot parameter template: %v\n----TEMPLATE---\n%s",
				err,
				b.BootParams)
		}
		b.bootParamsTmpl = tmpl.Option("missingkey=error")
	}
	return
}

// JoinInitrds joins the fully expanded initrd paths into a comma-separated string.
func (b *BootEnv) JoinInitrds(proto string) string {
	fullInitrds := make([]string, len(b.Initrds))
	for i, initrd := range b.Initrds {
		fullInitrds[i] = b.PathFor(proto, initrd)
	}
	return strings.Join(fullInitrds, " ")
}

func (b *BootEnv) prefix() string {
	return "bootenvs"
}

func (b *BootEnv) key() string {
	return path.Join(b.prefix(), b.Name)
}

func (b *BootEnv) tenantId() int {
	return b.TenantId
}

func (b *BootEnv) setTenantId(tid int) {
	b.TenantId = tid
}

func (b *BootEnv) typeName() string {
	return "BOOTENV"
}

func (b *BootEnv) newIsh() keySaver {
	res := &BootEnv{Name: b.Name}
	return keySaver(res)
}

// RenderPaths renders the paths of the templates for this machine.
func (b *BootEnv) RenderPaths(machine *Machine) error {
	vars := &RenderData{
		Machine:        machine,
		Env:            b,
		ProvisionerURL: provisionerURL,
		CommandURL:     commandURL,
		TenantId:       b.TenantId,
	}
	for _, templateParams := range b.Templates {
		pathBuf := &bytes.Buffer{}
		if err := templateParams.pathTmpl.Execute(pathBuf, vars); err != nil {
			b.Errorf("template: Error rendering path %s (%s): %v",
				templateParams.Name,
				templateParams.Path,
				err)
			continue
		}
		templateParams.finalPath = filepath.Join(fileRoot, pathBuf.String())
	}
	return b.errorOrNil()
}

// RenderTemplates renders the templates in the bootenv with the data from the machine.
func (b *BootEnv) RenderTemplates(machine *Machine) error {
	vars := &RenderData{
		Machine:        machine,
		Env:            b,
		ProvisionerURL: provisionerURL,
		CommandURL:     commandURL,
		TenantId:       b.TenantId,
	}
	b.parseTemplates()
	b.RenderPaths(machine)
	var missingParams []string
	for _, param := range b.RequiredParams {
		if _, ok := machine.Params[param]; !ok {
			missingParams = append(missingParams, param)
		}
	}
	if len(missingParams) > 0 {
		b.Errorf("bootenv: %s missing required machine params for %s:\n %v", b.Name, machine.Name, missingParams)
	}
	for _, templateParams := range b.Templates {
		tmplPath := templateParams.finalPath
		if err := os.MkdirAll(path.Dir(tmplPath), 0755); err != nil {
			b.Errorf("template: Unable to create dir for %s: %v", tmplPath, err)
			continue
		}

		tmplDest, err := os.Create(tmplPath)
		if err != nil {
			b.Errorf("template: Unable to create file %s: %v", tmplPath, err)
			continue
		}
		defer tmplDest.Close()
		if err := templateParams.contents.Render(tmplDest, vars); err != nil {
			os.Remove(tmplPath)
			b.Errorf("template: Error rendering template %s: %v\n---template---\n %s",
				templateParams.Name,
				err,
				templateParams.contents.Contents)
			continue
		}
		tmplDest.Sync()
	}
	return b.errorOrNil()
}

// DeleteRenderedTemplates deletes the templates that were rendered
// for this bootenv/machine combination.
func (b *BootEnv) DeleteRenderedTemplates(machine *Machine) {
	b.parseTemplates()
	b.RenderPaths(machine)
	for _, tmpl := range b.Templates {
		if tmpl.finalPath != "" {
			os.Remove(tmpl.finalPath)
		}
	}
}

func (b *BootEnv) explodeIso() error {
	// Only explode install things
	if !strings.HasSuffix(b.Name, "-install") {
		logger.Printf("Explode ISO: Skipping %s becausing not -install\n", b.Name)
		return nil
	}
	// Only work on things that are requested.
	if b.OS.IsoFile == "" {
		logger.Printf("Explode ISO: Skipping %s becausing no iso image specified\n", b.Name)
		return nil
	}
	// Have we already exploded this?  If file exists, then good!
	canaryPath := b.PathFor("disk", "."+b.OS.Name+".rebar_canary")
	if _, err := os.Stat(canaryPath); err == nil {
		logger.Printf("Explode ISO: Skipping %s becausing canary file, %s, in place\n", b.Name, canaryPath)
		return nil
	}

	isoPath := filepath.Join(fileRoot, "isos", b.OS.IsoFile)
	if _, err := os.Stat(isoPath); os.IsNotExist(err) {
		logger.Printf("Explode ISO: Skipping %s becausing iso doesn't exist: %s\n", b.Name, isoPath)
		return nil
	}

	// Sha256sum iso for correctness
	if b.OS.IsoSha256 != "" {
		f, err := os.Open(isoPath)
		if err != nil {
			return fmt.Errorf("Explode ISO: For %s, failed to open iso file %s: %v", b.Name, isoPath, err)
		} else {
			defer f.Close()
			hasher := sha256.New()
			if _, err := io.Copy(hasher, f); err != nil {
				return fmt.Errorf("Explode ISO: For %s, failed to read iso file %s: %v", b.Name, isoPath, err)
			}
			hash := hex.EncodeToString(hasher.Sum(nil))
			if hash != b.OS.IsoSha256 {
				return fmt.Errorf("iso: Iso checksum bad.  Re-download image: %s: actual: %v expected: %v", isoPath, hash, b.OS.IsoSha256)
			}
		}
	}

	// Call extract script
	// /explode_iso.sh b.OS.Name isoPath path.Dir(canaryPath)
	cmdName := "/explode_iso.sh"
	cmdArgs := []string{b.OS.Name, isoPath, path.Dir(canaryPath)}
	if _, err := exec.Command(cmdName, cmdArgs...).Output(); err != nil {
		return fmt.Errorf("Explode ISO: Exec command failed for %s: %s\n", b.Name, err)
	}
	return nil
}

func (b *BootEnv) getFile(f *FileData) error {
	logger.Printf("Downloading file: %s\n", f.Name)
	filePath := b.PathFor("disk", f.Name)
	if err := os.MkdirAll(path.Dir(filePath), 0755); err != nil {
		return fmt.Errorf("file: Unable to create dir for %s: %v", filePath, err)
	}

	fileDest, err := os.Create(filePath)
	if err != nil {
		return err
	}
	defer fileDest.Close()

	resp, err := http.Get(f.URL)
	if err != nil {
		return err
	}
	defer resp.Body.Close()

	_, err = io.Copy(fileDest, resp.Body)
	return err
}

func (b *BootEnv) validateFile(f *FileData) error {
	logger.Printf("Validating file: %s\n", f.Name)
	filePath := b.PathFor("disk", f.Name)
	if _, err := os.Stat(filePath); os.IsNotExist(err) {
		return fmt.Errorf("validate: File doesn't exist: %s\n", filePath)
	}
	return nil
}

func (b *BootEnv) onChange(oldThing interface{}) error {
	seenPxeLinux := false
	seenELilo := false
	seenIPXE := false
	b.Errors = []string{}
	for _, template := range b.Templates {
		if template.Name == "pxelinux" {
			seenPxeLinux = true
		}
		if template.Name == "elilo" {
			seenELilo = true
		}
		if template.Name == "ipxe" {
			seenIPXE = true
		}
		if template.Name == "" ||
			template.Path == "" ||
			template.UUID == "" {
			b.Errorf("bootenv: Illegal template: %+v", template)
		}
	}
	if !seenIPXE {
		if !(seenPxeLinux && seenELilo) {
			b.Errorf("bootenv: Missing elilo or pxelinux template")
		}
	}

	// Make sure the ISO is exploded
	if b.OS.IsoFile != "" {
		logger.Printf("Exploding ISO for %s\n", b.OS.Name)
		if err := b.explodeIso(); err != nil {
			b.Errorf("bootenv: Unable to expand ISO %s: %v", b.OS.IsoFile, err)
		}
	}

	// Make sure we download extra files
	for _, f := range b.OS.Files {
		if b.validateFile(f) != nil {
			if err := b.getFile(f); err != nil {
				b.Errorf("bootenv: Unable to download extra file %s: %v", f, err)
				continue
			}
		}
		if err := b.validateFile(f); err != nil {
			b.Errorf("bootenv: Unable to validate extra file %s: %v", f, err)
		}
	}
	b.parseTemplates()
	if b.Kernel != "" {
		kPath := b.PathFor("disk", b.Kernel)
		kernelStat, err := os.Stat(kPath)
		if err != nil {
			b.Errorf("bootenv: %s: missing kernel %s (%s)",
				b.Name,
				b.Kernel,
				kPath)
		} else if !kernelStat.Mode().IsRegular() {
			b.Errorf("bootenv: %s: invalid kernel %s (%s)",
				b.Name,
				b.Kernel,
				kPath)
		}
	}
	if len(b.Initrds) > 0 {
		for _, initrd := range b.Initrds {
			iPath := b.PathFor("disk", initrd)
			initrdStat, err := os.Stat(iPath)
			if err != nil {
				b.Errorf("bootenv: %s: missing initrd %s (%s)",
					b.Name,
					initrd,
					iPath)
				continue
			}
			if !initrdStat.Mode().IsRegular() {
				b.Errorf("bootenv: %s: invalid initrd %s (%s)",
					b.Name,
					initrd,
					iPath)
			}
		}
	}

	if old, ok := oldThing.(*BootEnv); ok && old != nil {
		if old.Name != b.Name {
			b.Errorf("bootenv: Cannot change name of bootenv %s", old.Name)
		}
		machine := &Machine{}
		machines, err := machine.List()
		if err != nil {
			b.Errorf("bootenv: Failed to get list of current machines: %v", err)
		}

		for _, machine := range machines {
			if machine.BootEnv != old.Name {
				continue
			}
			if err := b.RenderTemplates(machine); err != nil {
				b.Errorf("bootenv: Failed to render templates for machine %s: %v", machine.Name, err)
			}
		}
	}
	b.Available = (len(b.Errors) == 0)
	return nil
}

func (b *BootEnv) onDelete() error {
	b.Errors = []string{}
	machine := &Machine{}
	machines, err := machine.List()
	if err == nil {
		for _, machine := range machines {
			if machine.BootEnv != b.Name {
				continue
			}
			b.Errorf("Bootenv %s in use by Machine %s", b.Name, machine.Name)
		}
	}
	return b.errorOrNil()
}

func (b *BootEnv) List() ([]*BootEnv, error) {
	things := backend.list(b)
	res := make([]*BootEnv, len(things))
	for i, blob := range things {
		bootenv := &BootEnv{}
		if err := json.Unmarshal(blob, bootenv); err != nil {
			return nil, err
		}
		res[i] = bootenv
	}
	return res, nil
}

func (b *BootEnv) RebuildRebarData() error {
	preferredOses := map[string]int{
		"centos-7.2.1511": 0,
		"centos-7.1.1503": 1,
		"ubuntu-14.04":    2,
		"ubuntu-15.04":    3,
		"debian-8":        4,
		"centos-6.6":      5,
		"debian-7":        6,
		"redhat-6.5":      7,
		"ubuntu-12.04":    8,
	}

	attrValOSes := make(map[string]bool)
	attrValOS := "STRING"
	attrPref := 1000

	if !b.Available {
		return b.errorOrNil()
	}

	bes, err := b.List()
	if err != nil {
		return err
	}

	for _, be := range bes {
		if !strings.HasSuffix(be.Name, "-install") {
			continue
		}
		if !be.Available {
			continue
		}
		attrValOSes[be.OS.Name] = true
		numPref, ok := preferredOses[be.OS.Name]
		if !ok {
			numPref = 999
		}
		if numPref < attrPref {
			attrValOS = be.OS.Name
			attrPref = numPref
		}
	}

	deployment := &api.Deployment{}
	if err := rebarClient.Fetch(deployment, "system"); err != nil {
		return err
	}

	role := &api.Role{}
	if err := rebarClient.Fetch(role, "provisioner-service"); err != nil {
		return err
	}

	var tgt api.Attriber
	for {
		drs := []*api.DeploymentRole{}
		matcher := make(map[string]interface{})
		matcher["role_id"] = role.ID
		matcher["deployment_id"] = deployment.ID
		dr := &api.DeploymentRole{}
		if err := rebarClient.Match(rebarClient.UrlPath(dr), matcher, &drs); err != nil {
			return err
		}
		if len(drs) != 0 {
			tgt = drs[0]
			break
		}
		log.Printf("Waiting for provisioner-service (%v) to show up in system(%v)", role.ID, deployment.ID)
		log.Printf("drs: %#v, err: %#v", drs, err)
		time.Sleep(5 * time.Second)
	}

	attrib := &api.Attrib{}
	attrib.SetId("provisioner-available-oses")
	attrib, err = rebarClient.GetAttrib(tgt, attrib, "")
	if err != nil {
		return err
	}
	attrib.Value = attrValOSes
	if err := rebarClient.SetAttrib(tgt, attrib, ""); err != nil {
		return err
	}

	attrib = &api.Attrib{}
	attrib.SetId("provisioner-default-os")
	attrib, err = rebarClient.GetAttrib(tgt, attrib, "")
	if err != nil {
		return err
	}
	attrib.Value = attrValOS
	if err := rebarClient.SetAttrib(tgt, attrib, ""); err != nil {
		return err
	}

	if err := rebarClient.Commit(tgt); err != nil {
		return err
	}

	return nil
}
