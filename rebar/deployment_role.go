package main

import (
	"fmt"
	"log"

	"github.com/digitalrebar/rebar-api/client"
	"github.com/spf13/cobra"
)

func addDeploymentRolerCommands(singularName string,
	maker func() client.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(client.DeploymentRoler); !ok {
		return
	}
	cmd := &cobra.Command{
		Use:   "deploymentroles [id]",
		Short: fmt.Sprintf("List all deploymentroles for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(client.DeploymentRoler)
			if client.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			objs, err := client.DeploymentRoles(obj)
			if err != nil {
				log.Fatalf("Failed to get deploymentroles for %v(%v)\n", singularName, args[0])
			}
			fmt.Println(prettyJSON(objs))
		},
	}
	res.AddCommand(cmd)
}

func init() {
	maker := func() client.Crudder { return &client.DeploymentRole{} }
	singularName := "deploymentrole"
	deploymentRoles := makeCommandTree(singularName, maker)
	app.AddCommand(deploymentRoles)
}
