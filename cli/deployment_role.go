package main

import (
	"fmt"
	crowbar "github.com/VictorLowther/crowbar-api"
	"github.com/spf13/cobra"
	"log"
)

func addDeploymentRolerCommands(singularName string,
	maker func() crowbar.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(crowbar.DeploymentRoler); !ok {
		return
	}
	cmd := &cobra.Command{
		Use:   "deploymentroles [id]",
		Short: fmt.Sprintf("List all deploymentroles for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(crowbar.DeploymentRoler)
			if crowbar.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			objs, err := crowbar.DeploymentRoles(obj)
			if err != nil {
				log.Fatalf("Failed to get deploymentroles for %v(%v)\n", singularName, args[0])
			}
			fmt.Println(prettyJSON(objs))
		},
	}
	res.AddCommand(cmd)
}

func init() {
	lister := func() ([]crowbar.Crudder, error) {
		deploymentRoles, err := crowbar.DeploymentRoles()
		if err != nil {
			return nil, err
		}
		res := make([]crowbar.Crudder, len(deploymentRoles))
		for i := range deploymentRoles {
			res[i] = deploymentRoles[i]
		}
		return res, nil
	}
	maker := func() crowbar.Crudder { return &crowbar.DeploymentRole{} }
	singularName := "deploymentrole"
	deploymentRoles := makeCommandTree(singularName, lister, maker)
	app.AddCommand(deploymentRoles)
}
