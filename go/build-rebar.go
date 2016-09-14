package main

import "log"
import "errors"
import "io/ioutil"
import "os"
import "os/exec"
import "strings"
import "github.com/digitalrebar/digitalrebar/go/common/version"

func Exists(name string) (bool, error) {
	_, err := os.Stat(name)
	if os.IsNotExist(err) {
		return false, nil
	}
	return err == nil, err
}

func RunCmd(addVendorGoPath bool, additionalEnv []string, name string, arg ...string) (error, string, string) {
	cmd := exec.Command(name, arg...)

	if addVendorGoPath {
		env := os.Environ()
		index := -1

		for i, s := range env {
			if strings.HasPrefix(s, "GOPATH=") {
				index = i
				break
			}
		}

		if index == -1 {
			return errors.New("No GOPATH set"), "", ""
		}

		gp := env[index]
		parts := strings.SplitN(gp, "=", 2)
		env[index] = "GOPATH=" + parts[1] + "/vendor_src:" + parts[1]
		cmd.Env = env
	}
	if additionalEnv != nil {
		env := cmd.Env
		for _, s := range additionalEnv {
			env = append(env, s)
		}
		cmd.Env = env
	}

	cmdOut, _ := cmd.StdoutPipe()
	cmdErr, _ := cmd.StderrPipe()

	startErr := cmd.Start()
	if startErr != nil {
		return startErr, "", ""
	}

	// read stdout and stderr
	stdOutput, _ := ioutil.ReadAll(cmdOut)
	errOutput, _ := ioutil.ReadAll(cmdErr)

	err := cmd.Wait()
	return err, string(stdOutput), string(errOutput)

}

func main() {
	log.Printf("Building Version: %s\n", version.REBAR_VERSION)

	// These are the init steps!
	if b, err := Exists("glide.yaml"); !b {
		if err != nil {
			log.Fatalf("glide.yaml issue: %v", err)
		}
		log.Println("Linking to glide.yaml file")
		err = os.Link("src/github.com/digitalrebar/digitalrebar/go/glide.yaml", "glide.yaml")
		if err != nil {
			log.Fatalf("Failed to create link to glide.yaml: %v", err)
		}
	}
	if b, err := Exists("glide.lock"); !b {
		if err != nil {
			log.Fatalf("glide.lock issue: %v", err)
		}
		log.Println("Linking to glide.lock file")
		err = os.Link("src/github.com/digitalrebar/digitalrebar/go/glide.lock", "glide.lock")
		if err != nil {
			log.Fatalf("Failed to create link to glide.lock: %v", err)
		}
	}
	if b, err := Exists("vendor"); !b {
		if err != nil {
			log.Fatalf("vendor issue: %v", err)
		}
		err = os.Mkdir("vendor", 0755)
		if err != nil {
			log.Fatalf("Failed to create vendor directory: %v", err)
		}
	}
	if b, err := Exists("vendor_src"); !b {
		if err != nil {
			log.Fatalf("vendor_src issue: %v", err)
		}
		err = os.Mkdir("vendor_src", 0755)
		if err != nil {
			log.Fatalf("Failed to create vendor_src directory: %v", err)
		}
		err = os.Symlink("../vendor", "vendor_src/src")
		if err != nil {
			log.Fatalf("Failed to create vendor_src/src to vendor symlink: %v", err)
		}
	}

	// Run glide i to make sure we have all the dependencies.
	log.Printf("Running glide to pull in dependencies")
	err, stdOut, stdErr := RunCmd(false, nil, "glide", "i")
	if err != nil {
		log.Printf("glide return: %v\n", err)
		log.Printf("glide output: %v\n", stdOut)
		log.Printf("glide error: %v\n", stdErr)
		log.Fatalf("glide failed!!")
	}

	progs := []string{
		"github.com/digitalrebar/digitalrebar/go/certificates/sign-it",
		"github.com/digitalrebar/digitalrebar/go/certificates/trust-me",
		"github.com/digitalrebar/digitalrebar/go/rebar-dhcp",
		"github.com/digitalrebar/digitalrebar/go/rebar-dns-mgmt",
		"github.com/digitalrebar/digitalrebar/go/rule-engine",
		"github.com/digitalrebar/digitalrebar/go/rebar-rev-proxy",
		"github.com/digitalrebar/digitalrebar/go/rebar-api/rebar",
		"github.com/digitalrebar/digitalrebar/go/forwarder",
		"github.com/digitalrebar/digitalrebar/go/provisioner-mgmt",
	}
	envs := [][]string{
		[]string{
			"GOOS=linux",
			"GOARCH=amd64",
		},
		[]string{
			"GOOS=darwin",
			"GOARCH=amd64",
		},
	}
	for _, env := range envs {
		for _, prog := range progs {
			log.Printf("Running build for %s", prog)
			err, stdOut, stdErr = RunCmd(true, env, "go", "install", prog)
			if err != nil {
				log.Printf("build %s return: %v\n", prog, err)
				log.Printf("build %s output: %v\n", prog, stdOut)
				log.Printf("build %s error: %v\n", prog, stdErr)
				log.Fatalf("build failed: %s!!", prog)
			}
		}
	}

}
