package main

import (
	"fmt"
	"log"

	"github.com/digitalrebar/digitalrebar/go/rebar-api/api"
	"github.com/spf13/cobra"
)

func addGrouperCommands(singularName string,
	maker func() api.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(api.Grouper); !ok {
		return
	}
	res.AddCommand(&cobra.Command{
		Use:   "groups [id]",
		Short: fmt.Sprintf("List all groups for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(api.Grouper)
			if session.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			objs, err := session.Groups(obj)
			if err != nil {
				log.Fatalf("Failed to get groups for %v(%v)\n", singularName, args[0])
			}
			fmt.Println(prettyJSON(objs))
		},
	})
	res.AddCommand(&cobra.Command{
		Use:   "addgroup [id] to [groupID]",
		Short: fmt.Sprintf("Add group [groupID] to %v [id]", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 3 {
				log.Fatalf("%v requires 2 arguments", c.UseLine())
			}
			obj := maker().(api.Grouper)
			group := &api.Group{}
			if session.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			if session.SetId(group, args[2]) != nil {
				log.Fatalf("Failed to parse ID %v for a group\n", args[1])
			}
			groups, err := session.AddGroup(obj, group)
			if err != nil {
				log.Fatalf("Failed to add group %v to %v %v: %v", args[2], singularName, args[0], err)
			}
			fmt.Println(prettyJSON(groups))
		},
	})
	res.AddCommand(&cobra.Command{
		Use:   "removegroup [id] from [groupID]",
		Short: fmt.Sprintf("Remove group [groupID] from %v [id]", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 3 {
				log.Fatalf("%v requires 2 arguments", c.UseLine())
			}
			obj := maker().(api.Grouper)
			group := &api.Group{}
			if session.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			if session.SetId(group, args[2]) != nil {
				log.Fatalf("Failed to parse ID %v for a group\n", args[1])
			}
			groups, err := session.RemoveGroup(obj, group)
			if err != nil {
				log.Fatalf("Failed to remove group %v from %v %v: %v", args[2], singularName, args[0], err)
			}
			fmt.Println(prettyJSON(groups))
		},
	})
}

func init() {
	maker := func() api.Crudder { return &api.Group{} }
	groups := makeCommandTree("group", maker)
	app.AddCommand(groups)
}
