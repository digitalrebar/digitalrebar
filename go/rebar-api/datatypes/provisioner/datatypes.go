package provisioner

import "path"

// TemplateInfo holds information on the templates in the boot
// environment that will be expanded into files.

const API_PATH = "/provisioner"

type TemplateInfo struct {
	Name string // Name of the template
	Path string // A template that specifies how to create the final path the template should be written to.
	UUID string // The UUID of the template that should be expanded.
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
	TenantId       int
	Errors         []string
}

func (b *BootEnv) Id() (string, error) {
	return b.Name, nil
}

func (b *BootEnv) SetId(s string) error {
	b.Name = s
	return nil
}

func (b *BootEnv) ApiName() string {
	return "bootenvs"
}

func (b *Bootenv) ApiPath() string {
	return path.Join(API_PATH, b.ApiName())
}

// Template represents a template that will be associated with a boot environment.
type Template struct {
	UUID     string // UUID is a unique identifier for this template.
	Contents string // Contents is the raw template.
	TenantId int
}

func (t *Template) Id() (string, error) {
	return t.UUID, nil
}

func (t *Template) SetId(s string) error {
	t.UUID = s
	return nil
}

func (t *Template) ApiName() string {
	return "templates"
}

func (t *Template) ApiPath() string {
	return path.Join(API_PATH, t.ApiName())
}

// Machine represents a single bare-metal system that the provisioner
// should manage the boot environment for.
type Machine struct {
	Name     string                 // The FQDN of the machine.
	Uuid     string                 // the UUID of the machine
	Address  string                 // The IPv4 address that the machine PXE boots with.
	BootEnv  string                 // The boot environment that the machine should boot into.
	Params   map[string]interface{} // Any additional parameters that may be needed for template expansion.
	TenantId int
}

func (m *Machine) Id() (string, error) {
	return m.Uuid, nil
}

func (m *Machine) SetId(s string) error {
	m.Uuid = s
	return nil
}

func (m *Machine) ApiName() string {
	return "machines"
}

func (m *Machine) ApiPath() string {
	return path.Join(API_PATH, m.ApiName())
}
