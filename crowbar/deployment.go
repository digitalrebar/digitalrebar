package main

import (
	"fmt"
	"log"

	crowbar "github.com/VictorLowther/crowbar-api"
	"github.com/spf13/cobra"
)

func addDeploymenterCommands(singularName string,
	maker func() crowbar.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(crowbar.Deploymenter); !ok {
		return
	}
	cmd := &cobra.Command{
		Use:   "deployments [id]",
		Short: fmt.Sprintf("List all deployments for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(crowbar.Deploymenter)
			if crowbar.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			objs, err := crowbar.Deployments(obj)
			if err != nil {
				log.Fatalf("Failed to get deploymentss for %v(%v)\n", singularName, args[0])
			}
			fmt.Println(prettyJSON(objs))
		},
	}
	res.AddCommand(cmd)
}

func init() {
	maker := func() crowbar.Crudder { return &crowbar.Deployment{} }
	singularName := "deployment"
	deployments := makeCommandTree(singularName, maker)
	deployments.AddCommand(&cobra.Command{
		Use:   "bind [id] to [roleId]",
		Short: "Bind a deployment to a role",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 3 || args[1] != "to" {
				log.Fatalf("%v requires 2 arguments seperated by \"to\"", c.UseLine())
			}
			obj := &crowbar.Deployment{}
			if crowbar.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			role := &crowbar.Role{}
			if crowbar.SetId(role, args[2]) != nil {
				log.Fatalf("Failed to parse ID %v for a role\n", args[2])
			}
			if err := crowbar.Read(obj); err != nil {
				log.Fatalf("Failed to fetch Deployment: %v\n", err.Error())
			}
			if err := crowbar.Read(role); err != nil {
				log.Fatalf("Failed to fetch Role: %v\n", err.Error())
			}
			nr := &crowbar.DeploymentRole{}
			nr.RoleID = role.ID
			nr.DeploymentID = obj.ID
			if err := crowbar.Create(nr); err != nil {
				log.Fatalf("Failed to create deploymentrole for deployment:%v role:%v\n", args[0], args[2])
			}
			fmt.Println(prettyJSON(nr))
		},
	})

	deployments.AddCommand(&cobra.Command{
		Use:   "parent [id]",
		Short: "Get the parent deployment of this deployment",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := &crowbar.Deployment{}
			if crowbar.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			if crowbar.Read(obj) != nil {
				log.Fatalf("Failed to retrieve deployment %v\n", args[0])
			}
			if !obj.ParentID.Valid {
				log.Fatalf("System deployment does not have a parent")
				return
			}
			res, err := obj.Parent()
			if err != nil {
				log.Fatalf("Failed to get parent of deployment %v\n", args[0])
			}
			fmt.Println(prettyJSON(res))
		},
	})
	app.AddCommand(deployments)
}
