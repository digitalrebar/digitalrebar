package main

import (
	"fmt"
	crowbar "github.com/VictorLowther/crowbar-api"
	"github.com/spf13/cobra"
	"log"
)

func addHammererCommands(singularName string,
	maker func() crowbar.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(crowbar.Hammerer); !ok {
		return
	}
	cmd := &cobra.Command{
		Use:   "hammers [id]",
		Short: fmt.Sprintf("List all hammers for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(crowbar.Hammerer)
			if crowbar.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			objs, err := crowbar.Hammers(obj)
			if err != nil {
				log.Fatalf("Failed to get hammerss for %v(%v)\n", singularName, args[0])
			}
			fmt.Println(prettyJSON(objs))
		},
	}
	res.AddCommand(cmd)
}

func init() {
	lister := func() ([]crowbar.Crudder, error) {
		hammers, err := crowbar.Hammers()
		if err != nil {
			return nil, err
		}
		res := make([]crowbar.Crudder, len(hammers))
		for i := range hammers {
			res[i] = hammers[i]
		}
		return res, nil
	}
	maker := func() crowbar.Crudder { return &crowbar.Hammer{} }
	singularName := "hammer"
	app.AddCommand(makeCommandTree(singularName, lister, maker))
}
