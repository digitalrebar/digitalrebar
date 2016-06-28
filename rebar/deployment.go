package main

import (
	"fmt"
	"log"

	"github.com/digitalrebar/rebar-api/client"
	"github.com/spf13/cobra"
)

func addDeploymenterCommands(singularName string,
	maker func() client.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(client.Deploymenter); !ok {
		return
	}
	cmd := &cobra.Command{
		Use:   "deployments [id]",
		Short: fmt.Sprintf("List all deployments for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(client.Deploymenter)
			if client.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			objs, err := client.Deployments(obj)
			if err != nil {
				log.Fatalf("Failed to get deploymentss for %v(%v)\n", singularName, args[0])
			}
			fmt.Println(prettyJSON(objs))
		},
	}
	res.AddCommand(cmd)
}

func init() {
	maker := func() client.Crudder { return &client.Deployment{} }
	singularName := "deployment"
	deployments := makeCommandTree(singularName, maker)
	deployments.AddCommand(&cobra.Command{
		Use:   "bind [id] to [roleId]",
		Short: "Bind a deployment to a role",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 3 || args[1] != "to" {
				log.Fatalf("%v requires 2 arguments seperated by \"to\"", c.UseLine())
			}
			obj := &client.Deployment{}
			if client.Fetch(obj, args[0]) != nil {
				log.Fatalf("Failed to fetch %v\n", singularName)
			}
			role := &client.Role{}
			if client.Fetch(role, args[2]) != nil {
				log.Fatalf("Failed to fetch role\n")
			}
			nr := &client.DeploymentRole{}
			nr.RoleID = role.ID
			nr.DeploymentID = obj.ID
			if err := client.BaseCreate(nr); err != nil {
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
			obj := &client.Deployment{}
			if client.Fetch(obj, args[0]) != nil {
				log.Fatalf("Failed to fetch %v\n", singularName)
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
	deployments.AddCommand(&cobra.Command{
		Use:   "redeploy [id]",
		Short: "Force everything in a deployment to redeploy from scratch",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires one argument\n", c.UseLine())
			}
			obj := &client.Deployment{}
			if client.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for a DeploymentRole\n", args[0])
			}
			if err := obj.Redeploy(); err != nil {
				log.Fatalf("Failed to redeploy %v\n%v\n", args[0], err)
			}
			fmt.Println(prettyJSON(obj))
		},
	})
	app.AddCommand(deployments)
}
