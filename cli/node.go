package main

import (
	"encoding/json"
	"fmt"
	"log"

	crowbar "github.com/VictorLowther/crowbar-api"
	"github.com/spf13/cobra"
)

func addNoderCommands(singularName string,
	maker func() crowbar.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(crowbar.Noder); !ok {
		return
	}
	cmd := &cobra.Command{
		Use:   "nodes [id]",
		Short: fmt.Sprintf("List all nodes for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(crowbar.Noder)
			if crowbar.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			objs, err := crowbar.Nodes(obj)
			if err != nil {
				log.Fatalf("Failed to get nodes for %v(%v)\n", singularName, args[0])
			}
			fmt.Println(prettyJSON(objs))
		},
	}
	res.AddCommand(cmd)
}

func init() {
	lister := func() ([]crowbar.Crudder, error) {
		nodes, err := crowbar.Nodes()
		if err != nil {
			return nil, err
		}
		res := make([]crowbar.Crudder, len(nodes))
		for i := range nodes {
			res[i] = nodes[i]
		}
		return res, nil
	}
	matcher := func(sample string) (string, error) {
		obj := &crowbar.Node{}
		err := json.Unmarshal([]byte(sample), obj)
		if err != nil {
			return "", fmt.Errorf("Error unmarshalling node\nError: %v\n", err.Error())
		}
		objs, err := obj.Match()
		if err != nil {
			return "", fmt.Errorf("Error fetching matches for %v", sample)
		}
		return prettyJSON(objs), nil
	}
	maker := func() crowbar.Crudder { return &crowbar.Node{} }
	singularName := "node"
	nodes := makeCommandTree(singularName, lister, matcher, maker)
	nodes.AddCommand(&cobra.Command{
		Use:   "bind [id] to [roleId]",
		Short: "Bind a node to a role",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 3 || args[1] != "to" {
				log.Fatalf("%v requires 2 arguments seperated by \"to\"", c.UseLine())
			}
			obj := &crowbar.Node{}
			if crowbar.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			role := &crowbar.Role{}
			if crowbar.SetId(obj, args[2]) != nil {
				log.Fatalf("Failed to parse ID %v for a role\n", args[2])
			}
			if err := crowbar.Read(obj); err != nil {
				log.Fatalf("Failed to fetch Node: %v\n", err.Error())
			}
			if err := crowbar.Read(role); err != nil {
				log.Fatalf("Failed to fetch Role: %v\n", err.Error())
			}
			nr := &crowbar.NodeRole{RoleID: role.ID, NodeID: obj.ID}
			if err := crowbar.Create(nr); err != nil {
				log.Fatalf("Failed to create noderole for node:%v role:%v\n", args[0], args[2])
			}
			fmt.Println(prettyJSON(nr))
		},
	})
	nodes.AddCommand(&cobra.Command{
		Use:   "move [id] to [deployment]",
		Short: "Atomically move a node from one deployment to another",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 3 {
				log.Fatalf("%v requires 2 argument\n", c.UseLine())
			}
			obj := &crowbar.Node{}
			if crowbar.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			depl := &crowbar.Deployment{}
			if crowbar.SetId(depl, args[2]) != nil {
				log.Fatalf("Failed to parse ID %v for a Deployment", args[2])
			}
			if crowbar.Read(obj) != nil {
				log.Fatalf("%v is not a valid Node\n", args[0])
			}
			if crowbar.Read(depl) != nil {
				log.Fatalf("%v is not a valid Deployment\n", args[2])
			}
			if obj.Move(depl) != nil {
				log.Fatalf("Failed to move node %v to deployment %v\n", args[0], args[2])
			}
			fmt.Println(prettyJSON(obj))
		},
	})
	nodes.AddCommand(&cobra.Command{
		Use:   "poweractions [id]",
		Short: "Get the power actions for this node",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := &crowbar.Node{}
			if crowbar.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			res, err := obj.PowerActions()
			if err != nil {
				log.Fatalf("Failed to get power actions for node %v\n", args[0])
			}
			fmt.Println(prettyJSON(res))
		},
	})
	nodes.AddCommand(&cobra.Command{
		Use:   "power [id] [action]",
		Short: "Perform a power control action for this node",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 2 {
				log.Fatalf("%v requires 2 arguments\n", c.UseLine())
			}
			obj := &crowbar.Node{}
			if crowbar.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			err := obj.Power(args[1])
			if err != nil {
				log.Fatalf("Failed perform power action %v\n", args[1])
			}
		},
	})
	nodes.AddCommand(&cobra.Command{
		Use:   "activebootenv [id]",
		Short: "Get the boot environment that the provisioner has configured for the node.",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := &crowbar.Node{}
			if crowbar.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			fmt.Println(obj.ActiveBootstate())
		},
	})
	app.AddCommand(nodes)
}
