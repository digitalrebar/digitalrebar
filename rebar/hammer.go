package main

import (
	"fmt"
	"log"

	"github.com/digitalrebar/rebar-api/client"
	"github.com/spf13/cobra"
)

func addHammererCommands(singularName string,
	maker func() client.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(client.Hammerer); !ok {
		return
	}
	cmd := &cobra.Command{
		Use:   "hammers [id]",
		Short: fmt.Sprintf("List all hammers for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(client.Hammerer)
			if client.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			objs, err := client.Hammers(obj)
			if err != nil {
				log.Fatalf("Failed to get hammerss for %v(%v)\n", singularName, args[0])
			}
			fmt.Println(prettyJSON(objs))
		},
	}
	res.AddCommand(cmd)
}

func init() {
	maker := func() client.Crudder { return &client.Hammer{} }
	singularName := "hammer"
	app.AddCommand(makeCommandTree(singularName, maker))
}
