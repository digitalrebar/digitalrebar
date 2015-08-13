package main

import (
	"fmt"
	"log"

	"github.com/VictorLowther/crowbar-api/client"
	"github.com/spf13/cobra"
)

func addRolerCommands(singularName string,
	maker func() client.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(client.Roler); !ok {
		return
	}
	res.AddCommand(&cobra.Command{
		Use:   "roles [id]",
		Short: fmt.Sprintf("List all roles for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(client.Roler)
			if client.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			objs, err := client.Roles(obj)
			if err != nil {
				log.Fatalf("Failed to get roles for %v(%v)\n", singularName, args[0])
			}
			fmt.Println(prettyJSON(objs))
		},
	})
}

func init() {
	maker := func() client.Crudder { return &client.Role{} }
	singularName := "role"
	app.AddCommand(makeCommandTree(singularName, maker))
}
