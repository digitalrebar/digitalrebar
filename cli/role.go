package main

import (
	"fmt"
	crowbar "github.com/VictorLowther/crowbar-api"
	"github.com/spf13/cobra"
	"log"
)

func addRolerCommands(singularName string,
	maker func() crowbar.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(crowbar.Roler); !ok {
		return
	}
	res.AddCommand(&cobra.Command{
		Use:   "roles [id]",
		Short: fmt.Sprintf("List all roles for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(crowbar.Roler)
			if crowbar.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			objs, err := crowbar.Roles(obj)
			if err != nil {
				log.Fatalf("Failed to get roles for %v(%v)\n", singularName, args[0])
			}
			fmt.Println(prettyJSON(objs))
		},
	})
}

func init() {
	lister := func() ([]crowbar.Crudder, error) {
		roles, err := crowbar.Roles()
		if err != nil {
			return nil, err
		}
		res := make([]crowbar.Crudder, len(roles))
		for i := range roles {
			res[i] = roles[i]
		}
		return res, nil
	}
	maker := func() crowbar.Crudder { return &crowbar.Role{} }
	singularName := "role"
	app.AddCommand(makeCommandTree(singularName, lister, maker))
}
