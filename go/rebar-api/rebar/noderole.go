package main

import (
	"fmt"
	"log"

	"github.com/digitalrebar/digitalrebar/go/rebar-api/api"
	"github.com/spf13/cobra"
)

func init() {
	maker := func() api.Crudder { return &api.NodeRole{} }
	singularName := "noderole"
	nodes := makeCommandTree(singularName, maker)
	nodes.AddCommand(&cobra.Command{
		Use:   "retry [id]",
		Short: "Force a node role to rerun itself",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires one argument\n", c.UseLine())
			}
			obj := &api.NodeRole{}
			if session.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for a NodeRole\n", args[0])
			}
			if err := obj.Retry(); err != nil {
				log.Fatalf("Failed to retry %v\n%v\n", args[0], err)
			}
			fmt.Println(prettyJSON(obj))
		},
	})
	app.AddCommand(nodes)
}
