package main

import (
	"fmt"
	"log"

	"github.com/rackn/digitalrebar/go/rebar-api/api"
	"github.com/spf13/cobra"
)

func addProvidererCommands(singularName string,
	maker func() api.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(api.Providerer); !ok {
		return
	}
	cmd := &cobra.Command{
		Use:   "providers [id]",
		Short: fmt.Sprintf("List all providers for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(api.Providerer)
			if session.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			objs, err := session.Providers(obj)
			if err != nil {
				log.Fatalf("Failed to get providerss for %v(%v)\n", singularName, args[0])
			}
			fmt.Println(prettyJSON(objs))
		},
	}
	res.AddCommand(cmd)
}

func init() {
	maker := func() api.Crudder { return &api.Provider{} }
	singularName := "provider"
	app.AddCommand(makeCommandTree(singularName, maker))
}
