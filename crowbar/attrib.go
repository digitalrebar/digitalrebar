package main

import (
	"encoding/json"
	"fmt"
	"log"

	"github.com/VictorLowther/crowbar-api/client"
	"github.com/spf13/cobra"
)

func addAttriberCommands(singularName string,
	maker func() client.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(client.Attriber); !ok {
		return
	}
	commands := make([]*cobra.Command, 5)
	commands[0] = &cobra.Command{
		Use:   "attribs [id]",
		Short: fmt.Sprintf("List all attribs for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(client.Attriber)
			if client.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			objs, err := client.Attribs(obj)
			if err != nil {
				log.Fatalf("Failed to get attribs for %v(%v)\n", singularName, args[0])
			}
			fmt.Println(prettyJSON(objs))
		},
	}
	commands[1] = &cobra.Command{
		Use:   "get [id] attrib [attrib] bucket [bucket=all]",
		Short: fmt.Sprintf("Get an attrib for a specific %v in a specific bucket.", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 3 && len(args) != 5 {
				log.Fatalf("%v requires 2 or 3 arguments.\n", c.UseLine())
			}
			obj := maker().(client.Attriber)
			attr := &client.Attrib{}
			if client.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			if client.SetId(attr, args[2]) != nil {
				log.Fatalf("Failed to parse ID %v for an attrib\n", args[1])
			}
			bucket := "all"
			if len(args) == 5 {
				bucket = args[4]
			}
			attr, err := client.GetAttrib(obj, attr, bucket)
			if err != nil {
				log.Fatalf("Error fetching attrib %v for %v(%v): %v\n", args[1], singularName, args[0], err.Error())
			}
			fmt.Println(prettyJSON(attr))
		},
	}
	commands[2] = &cobra.Command{
		Use:   "set [id] attrib [attrib] to [json] bucket [bucket=user]",
		Short: fmt.Sprintf("Set an attrib value for a specific %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			bucket := "user"
			if len(args) != 5 && len(args) != 7 {
				log.Fatalf("%v requires 3 or 4 arguments.\n", c.UseLine())
			}
			obj := maker().(client.Attriber)
			attr := &client.Attrib{}
			if len(args) == 7 {
				bucket = args[6]
			}
			if client.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			if client.SetId(attr, args[2]) != nil {
				log.Fatalf("Failed to parse ID %v for an attrib\n", args[1])
			}
			attr, err := client.GetAttrib(obj, attr, "")
			if err != nil {
				log.Fatalf("Failed to fetch Attrib from server\n%v", err)
			}
			type Valuer struct {
				Value interface{} `json:"value"`
			}
			val := &Valuer{}
			err = json.Unmarshal([]byte(args[4]), val)
			if err != nil {
				log.Fatalf("Failed to parse %v as JSON!\nError: %v\n", args[2], err.Error())
			}
			attr.Value = val.Value
			if err := client.SetAttrib(obj, attr, bucket); err != nil {
				log.Fatalf("Unable to set attrib! Error: %v\n", err.Error())
			}
			fmt.Println(prettyJSON(attr))
		},
	}
	commands[3] = &cobra.Command{
		Use:   "propose [id]",
		Short: fmt.Sprintf("Tell a specific %v to prepare to accept new attrib values", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(client.Attriber)
			if client.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			if err := client.Propose(obj); err != nil {
				log.Fatalf("Failed to propose: %v\n", err.Error())
			}
			fmt.Println(prettyJSON(obj))
		},
	}
	commands[4] = &cobra.Command{
		Use:   "commit [id]",
		Short: fmt.Sprintf("Have %v commit any new attrib values", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(client.Attriber)
			if client.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			if err := client.Commit(obj); err != nil {
				log.Fatalf("Failed to commit: %v\n", err.Error())
			}
			fmt.Println(prettyJSON(obj))
		},
	}
	res.AddCommand(commands...)
}

func init() {
	maker := func() client.Crudder { return &client.Attrib{} }
	app.AddCommand(makeCommandTree("attrib", maker))
}
