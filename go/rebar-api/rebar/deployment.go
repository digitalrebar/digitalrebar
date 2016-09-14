package main

import (
	"fmt"
	"log"

	"github.com/rackn/digitalrebar/go/rebar-api/api"
	"github.com/spf13/cobra"
)

func addDeploymenterCommands(singularName string,
	maker func() api.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(api.Deploymenter); !ok {
		return
	}
	cmd := &cobra.Command{
		Use:   "deployments [id]",
		Short: fmt.Sprintf("List all deployments for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(api.Deploymenter)
			if session.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			objs, err := session.Deployments(obj)
			if err != nil {
				log.Fatalf("Failed to get deploymentss for %v(%v)\n", singularName, args[0])
			}
			fmt.Println(prettyJSON(objs))
		},
	}
	res.AddCommand(cmd)
}

func init() {
	maker := func() api.Crudder { return &api.Deployment{} }
	singularName := "deployment"
	deployments := makeCommandTree(singularName, maker)
	deployments.AddCommand(&cobra.Command{
		Use:   "bind [id] to [roleId]",
		Short: "Bind a deployment to a role",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 3 || args[1] != "to" {
				log.Fatalf("%v requires 2 arguments seperated by \"to\"", c.UseLine())
			}
			obj := &api.Deployment{}
			if session.Fetch(obj, args[0]) != nil {
				log.Fatalf("Failed to fetch %v\n", singularName)
			}
			role := &api.Role{}
			if session.Fetch(role, args[2]) != nil {
				log.Fatalf("Failed to fetch role\n")
			}
			nr := &api.DeploymentRole{}
			nr.RoleID = role.ID
			nr.DeploymentID = obj.ID
			if err := session.BaseCreate(nr); err != nil {
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
			obj := &api.Deployment{}
			if session.Fetch(obj, args[0]) != nil {
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
			obj := &api.Deployment{}
			if session.SetId(obj, args[0]) != nil {
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
