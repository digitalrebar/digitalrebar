package main

import (
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	"os"

	"github.com/digitalrebar/digitalrebar/go/rebar-api/api"
	"github.com/spf13/cobra"
)

func init() {
	provisioner := &cobra.Command{
		Use:   "provisioner",
		Short: "Commands related to the provisioner service",
	}
	app.AddCommand(provisioner)
	bootEnv := &cobra.Command{
		Use:   "bootenvs",
		Short: "Commands to manipulate provisioner boot environments",
	}
	provisioner.AddCommand(bootEnv)
	bootEnv.AddCommand(&cobra.Command{
		Use:   "list",
		Short: "List all provisioner boot environments",
		Run: func(c *cobra.Command, args []string) {
			objs := []interface{}{}
			obj := &api.ProvisionerBootEnv{}
			if err := session.List(session.UrlPath(obj), &objs); err != nil {
				log.Fatalf("Error listing provisioner boot environments: %v", err)
			}
			fmt.Println(prettyJSON(objs))
		},
	})
	bootEnv.AddCommand(&cobra.Command{
		Use:   "show [name]",
		Short: "Show the provisioner boot environment with the specific name",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument", c.UseLine())
			}
			obj := &api.ProvisionerBootEnv{}
			if err := session.Fetch(obj, args[0]); err != nil {
				log.Fatalf("Failed to fetch provisioner boot environment %v: %v", args[0], err)
			}
			fmt.Println(prettyJSON(obj))
		},
	})
	bootEnv.AddCommand(&cobra.Command{
		Use:   "create [json]",
		Short: "Create a new provisioner boot environment with the passed-in JSON",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument", c.UseLine())
			}
			obj := &api.ProvisionerBootEnv{}
			if err := session.Import(obj, []byte(args[0])); err != nil {
				log.Fatalf("Unable to create new boot environment: %v", err)
			}
			fmt.Println(prettyJSON(obj))
		},
	})
	bootEnv.AddCommand(&cobra.Command{
		Use:   "update [name] [json]",
		Short: "Update the named boot environment with the passed-in JSON",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 2 {
				log.Fatalf("%v requires 2 arguments", c.UseLine())
			}
			obj := &api.ProvisionerBootEnv{}
			if err := session.Fetch(obj, args[0]); err != nil {
				log.Fatalf("Failed to fetch boot environment: %v", err)
			}
			if err := session.UpdateJSON(obj, []byte(args[1])); err != nil {
				log.Fatalf("Unable to patch boot environment %v: %v", args[0], err)
			}
			fmt.Println(prettyJSON(obj))
		},
	})
	bootEnv.AddCommand(&cobra.Command{
		Use:   "destroy [name]",
		Short: "Destroy named provisioner boot environment",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("destroy required 1 argument")
			}
			obj := &api.ProvisionerBootEnv{}
			if session.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for a provisioner boot environment", args[0])
			}
			if err := session.Destroy(obj); err != nil {
				log.Fatalf("Unable to destroy boot environment %v: %v", args[0], err)
			}
			fmt.Printf("Deleted %v\n", args[0])
		},
	})
	template := &cobra.Command{
		Use:   "templates",
		Short: "Commands to manipulate provisioner templates",
	}
	provisioner.AddCommand(template)
	template.AddCommand(&cobra.Command{
		Use:   "list",
		Short: "List all provisioner templates",
		Run: func(c *cobra.Command, args []string) {
			objs := []interface{}{}
			obj := &api.ProvisionerTemplate{}
			if err := session.List(session.UrlPath(obj), &objs); err != nil {
				log.Fatalf("Error listing provisioner templates: %v", err)
			}
			fmt.Println(prettyJSON(objs))
		},
	})
	template.AddCommand(&cobra.Command{
		Use:   "show [uuid]",
		Short: "Show the provisioner template with the specific name as JSON",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument", c.UseLine())
			}
			obj := &api.ProvisionerTemplate{}
			if err := session.Fetch(obj, args[0]); err != nil {
				log.Fatalf("Failed to fetch provisioner template%v: %v", args[0], err)
			}
			fmt.Println(prettyJSON(obj))
		},
	})
	template.AddCommand(&cobra.Command{
		Use:   "dump [uuid]",
		Short: "Show the provisioner template with the specific name as raw text",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument", c.UseLine())
			}
			obj := &api.ProvisionerTemplate{}
			if err := session.Fetch(obj, args[0]); err != nil {
				log.Fatalf("Failed to fetch provisioner template%v: %v", args[0], err)
			}
			fmt.Println(obj.Contents)
		},
	})

	template.AddCommand(&cobra.Command{
		Use:   "create [json]",
		Short: "Create a new provisioner template with the passed-in JSON",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument", c.UseLine())
			}
			obj := &api.ProvisionerTemplate{}
			if err := session.Import(obj, []byte(args[0])); err != nil {
				log.Fatalf("Unable to create new template: %v", err)
			}
			fmt.Println(prettyJSON(obj))
		},
	})
	template.AddCommand(&cobra.Command{
		Use:   "upload [file] as [uuid]",
		Short: "Upload a template from the local filesystem, and give it the uuid",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 3 {
				log.Fatalf("%v requires 2 arguments", c.UseLine())
			}
			obj := &api.ProvisionerTemplate{}
			if session.SetId(obj, args[2]) != nil {
				log.Fatalf("Failed to parse ID %v for a provisioner template", args[2])
			}
			buf, err := ioutil.ReadFile(args[0])
			if err != nil {
				log.Fatalf("Unable to read %s: %v", args[0], err)
			}
			obj.Contents = string(buf)
			if err := session.BaseCreate(obj); err != nil {
				log.Fatalf("Unable to create or update template %s: %v", args[2], err)
			}
			fmt.Println(prettyJSON(obj))
		},
	})
	template.AddCommand(&cobra.Command{
		Use:   "update [uuid] [json]",
		Short: "Update the identified template with the passed-in JSON",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 2 {
				log.Fatalf("%v requires 2 arguments", c.UseLine())
			}
			obj := &api.ProvisionerTemplate{}
			if err := session.Fetch(obj, args[0]); err != nil {
				log.Fatalf("Failed to fetch template: %v", err)
			}
			if err := session.UpdateJSON(obj, []byte(args[1])); err != nil {
				log.Fatalf("Unable to patch template %v: %v", args[0], err)
			}
			fmt.Println(prettyJSON(obj))
		},
	})
	template.AddCommand(&cobra.Command{
		Use:   "destroy [uuid]",
		Short: "Destroy identified provisioner template",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("destroy requires 1 argument")
			}
			obj := &api.ProvisionerTemplate{}
			if session.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for a provisioner template", args[0])
			}
			if err := session.Destroy(obj); err != nil {
				log.Fatalf("Unable to destroy template %v: %v", args[0], err)
			}
			fmt.Printf("Deleted %v\n", args[0])
		},
	})
	machine := &cobra.Command{
		Use:   "machines",
		Short: "Commands to manipulate provisioner machines",
	}
	provisioner.AddCommand(machine)
	machine.AddCommand(&cobra.Command{
		Use:   "list",
		Short: "List all provisioner machines",
		Run: func(c *cobra.Command, args []string) {
			objs := []interface{}{}
			obj := &api.ProvisionerMachine{}
			if err := session.List(session.UrlPath(obj), &objs); err != nil {
				log.Fatalf("Error listing provisioner machines: %v", err)
			}
			fmt.Println(prettyJSON(objs))
		},
	})
	machine.AddCommand(&cobra.Command{
		Use:   "show [uuid]",
		Short: "Show the provisioner machine with the specific name as JSON",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument", c.UseLine())
			}
			obj := &api.ProvisionerMachine{}
			if err := session.Fetch(obj, args[0]); err != nil {
				log.Fatalf("Failed to fetch provisioner machine%v: %v", args[0], err)
			}
			fmt.Println(prettyJSON(obj))
		},
	})
	isos := &cobra.Command{
		Use:   "isos",
		Short: "Commands to manage ISO files on the provisioner",
	}
	provisioner.AddCommand(isos)
	isos.AddCommand(&cobra.Command{
		Use:   "list",
		Short: "List all uploaded ISOs",
		Run: func(c *cobra.Command, args []string) {
			objs := []string{}
			obj := &api.ProvisionerIso{}
			req, err := http.NewRequest("GET", session.UrlFor(obj, "isos"), nil)
			if err != nil {
				log.Fatalf("Error creating HTTP request: %v", err)
			}
			req.Header.Set("Accept", "application/json")
			req.Header.Set("Content-Type", "application/json")
			resp, err := session.BasicRequest(req)
			if err != nil {
				log.Fatalf("Error listing provisioner ISO files: %v", err)
			}

			buf, err := ioutil.ReadAll(resp.Body)
			if err != nil {
				log.Fatalf("Error listing provisioner ISO files: %v", err)
			}
			err = json.Unmarshal(buf, &objs)
			if err != nil {
				log.Fatalf("Error listing provisioner ISO files: %v", err)
			}
			resp.Body.Close()
			fmt.Println(prettyJSON(objs))
		},
	})
	isos.AddCommand(&cobra.Command{
		Use:   "download [name] to [file]",
		Short: "Download the ISO with [name] to [file]",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 3 {
				log.Fatalf("%v requires 2 arguments", c.UseLine())
			}
			tgt, err := os.Create(args[2])
			if err != nil {
				log.Fatalf("Error fetching ISO file: %v", err)
			}
			obj := &api.ProvisionerIso{}
			req, err := http.NewRequest("GET", session.UrlFor(obj, "isos", args[0]), nil)
			if err != nil {
				log.Fatalf("Error creating HTTP request: %v", err)
			}
			req.Header.Set("Content-Type", "application/json")
			req.Header.Set("Accept", "application/octet-stream")
			resp, err := session.BasicRequest(req)
			if err != nil {
				log.Fatalf("Error fetching ISO file: %v", err)
			}
			if resp.StatusCode >= 300 {
				resp.Body.Close()
				log.Fatalf("Error fetching ISO file: %v", resp.Status)
			}
			copied, err := io.Copy(tgt, resp.Body)
			if err != nil {
				os.Remove(args[2])
				log.Fatalf("Failed saving ISO file: %v", err)
			}
			if resp.ContentLength != -1 && resp.ContentLength != copied {
				os.Remove(args[2])
				log.Fatalf("Download of ISO file %s interrupted, %d/%d bytes saved", args[2], copied, resp.ContentLength)
			}
			resp.Body.Close()
			tgt.Close()
			fmt.Printf("ISO %s downloaded to %s", args[0], args[2])
		},
	})
	isos.AddCommand(&cobra.Command{
		Use:   "upload [file] as [name]",
		Short: " Upload [file] to the provisioner with [name]",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 3 {
				log.Fatalf("%v requires 2 arguments", c.UseLine())
			}
			obj := &api.ProvisionerIso{}
			src, err := os.Open(args[0])
			if err != nil {
				log.Fatalf("Cannot open ISO %s for upload: %v", args[0], err)
			}
			s, err := src.Stat()
			if err != nil {
				log.Fatalf("Cannot stat ISO %s for upload: %v", args[0], err)
			}
			req, err := http.NewRequest("POST", session.UrlFor(obj, "isos", args[2]), nil)
			if err != nil {
				log.Fatalf("Error creating HTTP request: %v", err)
			}
			req.Header.Set("Content-Type", "application/octet-stream")
			req.Header.Set("Accept", "application/json")
			req.ContentLength = s.Size()
			req.Body = src
			resp, err := session.BasicRequest(req)
			src.Close()
			if err != nil {
				log.Fatalf("Failed to upload file: %v", err)
			}
			if resp.StatusCode >= 300 {
				log.Fatalf("Failed to upload file: %v", resp.Status)
			}
			resp.Body.Close()
			fmt.Printf("ISO %s uploaded to %s", args[0], args[2])
		},
	})
	isos.AddCommand(&cobra.Command{
		Use:   "destroy [name]",
		Short: "Destroy ISO file with [name]",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 arg", c.UseLine())
			}
			obj := &api.ProvisionerIso{}
			req, err := http.NewRequest("DELETE", session.UrlFor(obj, "isos", args[0]), nil)
			req.Header.Set("Accept", "application/json")
			req.Header.Set("Content-Type", "application/json")
			resp, err := session.BasicRequest(req)
			if err != nil {
				log.Fatalf("Failed to delete ISO %s: %v", args[0], err)
			}
			if resp.StatusCode >= 300 {
				log.Fatalf("Failed to delete ISO %s: %v", args[0], resp.Status)
			}
			fmt.Printf("ISO %s deleted", args[0])
		},
	})
}
