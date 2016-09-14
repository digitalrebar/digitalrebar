package main

import (
	"fmt"
	"log"

	"github.com/rackn/digitalrebar/go/rebar-api/api"
	"github.com/spf13/cobra"
)

func addHammererCommands(singularName string,
	maker func() api.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(api.Hammerer); !ok {
		return
	}
	cmd := &cobra.Command{
		Use:   "hammers [id]",
		Short: fmt.Sprintf("List all hammers for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(api.Hammerer)
			if session.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			objs, err := session.Hammers(obj)
			if err != nil {
				log.Fatalf("Failed to get hammerss for %v(%v)\n", singularName, args[0])
			}
			fmt.Println(prettyJSON(objs))
		},
	}
	res.AddCommand(cmd)
}

func init() {
	maker := func() api.Crudder { return &api.Hammer{} }
	singularName := "hammer"
	app.AddCommand(makeCommandTree(singularName, maker))
}
