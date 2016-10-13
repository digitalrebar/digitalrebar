package main

import (
	"fmt"
	"io/ioutil"
	"log"

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
			if err := session.List(obj.ApiPath(), &objs); err != nil {
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
			if err := session.List(obj.ApiPath(), &objs); err != nil {
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
			if err := session.List(obj.ApiPath(), &objs); err != nil {
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
}
