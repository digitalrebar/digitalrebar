package main

import (
	"fmt"
	"log"

	"github.com/digitalrebar/digitalrebar/go/rebar-api/api"
	"github.com/spf13/cobra"
)

func addRolerCommands(singularName string,
	maker func() api.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(api.Roler); !ok {
		return
	}
	res.AddCommand(&cobra.Command{
		Use:   "roles [id]",
		Short: fmt.Sprintf("List all roles for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(api.Roler)
			if session.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			objs, err := session.Roles(obj)
			if err != nil {
				log.Fatalf("Failed to get roles for %v(%v)\n", singularName, args[0])
			}
			fmt.Println(prettyJSON(objs))
		},
	})
}

func init() {
	maker := func() api.Crudder { return &api.Role{} }
	singularName := "role"
	app.AddCommand(makeCommandTree(singularName, maker))
}
