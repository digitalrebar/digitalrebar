package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"strings"

	"github.com/digitalrebar/digitalrebar/go/rebar-api/api"
	"github.com/spf13/cobra"
)

func init() {
	dhcp := &cobra.Command{
		Use:   "dhcp",
		Short: "Commands related to the dhcpservice",
	}
	app.AddCommand(dhcp)
	subnet := &cobra.Command{
		Use:   "subnets",
		Short: "Commands to manipulate dhcp subnets",
	}
	dhcp.AddCommand(subnet)
	subnet.AddCommand(&cobra.Command{
		Use:   "list",
		Short: "List all dhcp boot environments",
		Run: func(c *cobra.Command, args []string) {
			objs := []interface{}{}
			obj := &api.DhcpSubnet{}
			if err := session.List(session.UrlPath(obj), &objs); err != nil {
				log.Fatalf("Error listing dhcp boot environments: %v", err)
			}
			fmt.Println(prettyJSON(objs))
		},
	})
	subnet.AddCommand(&cobra.Command{
		Use:   "show [name]",
		Short: "Show the dhcp boot environment with the specific name",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument", c.UseLine())
			}

			obj := &api.DhcpSubnet{}
			if err := session.Fetch(obj, args[0]); err != nil {
				log.Fatalf("Failed to fetch dhcp boot environment %v: %v", args[0], err)
			}
			fmt.Println(prettyJSON(obj))
		},
	})
	subnet.AddCommand(&cobra.Command{
		Use:   "create [json]",
		Short: "Create a new dhcp subnet with the passed-in JSON",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument", c.UseLine())
			}
			var buf []byte
			var err error
			if args[0] == "-" {
				buf, err = ioutil.ReadAll(os.Stdin)
				if err != nil {
					log.Fatalf("Error reading from stdin: %v", err)
				}
			} else {
				buf = []byte(args[0])
			}
			obj := &api.DhcpSubnet{}
			if err := session.Import(obj, buf); err != nil {
				log.Fatalf("Unable to create new subnet: %v", err)
			}
			fmt.Println(prettyJSON(obj))
		},
	})
	subnet.AddCommand(&cobra.Command{
		Use:   "update [name] [json]",
		Short: "Update the named subnet with the passed-in JSON",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 2 {
				log.Fatalf("%v requires 2 arguments", c.UseLine())
			}
			var buf []byte
			var err error
			if args[1] == "-" {
				buf, err = ioutil.ReadAll(os.Stdin)
				if err != nil {
					log.Fatalf("Error reading from stdin: %v", err)
				}
			} else {
				buf = []byte(args[1])
			}
			obj := &api.DhcpSubnet{}
			if err := session.Fetch(obj, args[0]); err != nil {
				log.Fatalf("Failed to fetch subnet: %v", err)
			}
			if err := session.UpdateJSON(obj, buf); err != nil {
				log.Fatalf("Unable to patch subnet %v: %v", args[0], err)
			}
			fmt.Println(prettyJSON(obj))
		},
	})
	subnet.AddCommand(&cobra.Command{
		Use:   "destroy [name]",
		Short: "Destroy named dhcp subnet",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v required 1 argument", c.UseLine())
			}
			obj := &api.DhcpSubnet{}
			if session.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for a dhcp subnet", args[0])
			}
			if err := session.Destroy(obj); err != nil {
				log.Fatalf("Unable to destroy subnet %v: %v", args[0], err)
			}
			fmt.Printf("Deleted %v\n", args[0])
		},
	})
	subnet.AddCommand(&cobra.Command{
		Use:   "bind [name] to [binding-spec]",
		Short: "Bind [binding-spec] JSON to the named subnet",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 3 {
				log.Fatalf("%v requires 2 arguments", c.UseLine())
			}
			obj := &api.DhcpSubnet{}
			if session.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for a dhcp subnet", args[0])
			}
			req, err := http.NewRequest("POST", session.UrlTo(obj, "bind"), strings.NewReader(args[2]))
			if err != nil {
				log.Fatalf("Failed to create HTTP request: %v", err)
			}
			req.Header.Set("Accept", "application/json")
			req.Header.Set("Content-Type", "application/json")
			resp, err := session.BasicRequest(req)
			req.Body.Close()
			if err != nil {
				log.Fatalf("Error creating binding for %s: %v", args[0], err)
			}
			resp.Body.Close()
			if resp.StatusCode >= 300 {
				log.Fatalf("Error creating binding for %s: %s", args[0], resp.Status)
			}
			fmt.Printf("%s: Binding created", args[0])
		},
	})
	subnet.AddCommand(&cobra.Command{
		Use:   "unbind [name] from [macaddr]",
		Short: "Unbind address binding with [macaddr] from subnet [name]",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 3 {
				log.Fatalf("%v requires 2 args", c.UseLine())
			}
			obj := &api.DhcpSubnet{}
			if session.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for a dhcp subnet", args[0])
			}
			req, err := http.NewRequest("DELETE", session.UrlTo(obj, "bind", args[2]), nil)
			if err != nil {
				log.Fatalf("Failed to create HTTP request: %v", err)
			}
			resp, err := session.BasicRequest(req)
			if err != nil {
				log.Fatalf("Error deleting binding %s from %s: %v", args[2], args[0], err)
			}
			resp.Body.Close()
			if resp.StatusCode >= 300 {
				log.Fatalf("Error deleting binding for %s: %s", args[0], resp.Status)
			}
			fmt.Printf("Deleted binding %s in %s", args[2], args[0])
		},
	})
	subnet.AddCommand(&cobra.Command{
		Use:   "nextserver [name] is [address]",
		Short: "Set the next-server parameter for subnet [name] to [address]",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 3 {
				log.Fatalf("%v requires 2 args", c.UseLine())
			}
			obj := &api.DhcpSubnet{}
			if session.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for a dhcp subnet", args[0])
			}
			req, err := http.NewRequest("PUT", session.UrlTo(obj, "next_server", args[2]), nil)
			if err != nil {
				log.Fatalf("Failed to create HTTP request: %v", err)
			}
			resp, err := session.BasicRequest(req)
			if err != nil {
				log.Fatalf("Error changing next server to %s for %s: %v", args[2], args[0], err)
			}
			resp.Body.Close()
			if resp.StatusCode >= 300 {
				log.Fatalf("Error changing next-server for %s: %s", args[0], resp.Status)
			}
			fmt.Printf("next-server updated %s in %s", args[2], args[0])
		},
	})
}
