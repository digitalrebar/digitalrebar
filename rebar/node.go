package main

import (
	"fmt"
	"log"

	"github.com/digitalrebar/rebar-api/api"
	"github.com/spf13/cobra"
)

func addNoderCommands(singularName string,
	maker func() api.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(api.Noder); !ok {
		return
	}
	cmd := &cobra.Command{
		Use:   "nodes [id]",
		Short: fmt.Sprintf("List all nodes for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(api.Noder)
			if session.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			objs, err := session.Nodes(obj)
			if err != nil {
				log.Fatalf("Failed to get nodes for %v(%v)\n", singularName, args[0])
			}
			fmt.Println(prettyJSON(objs))
		},
	}
	res.AddCommand(cmd)
}

func init() {
	maker := func() api.Crudder { return &api.Node{} }
	singularName := "node"
	nodes := makeCommandTree(singularName, maker)
	nodes.AddCommand(&cobra.Command{
		Use:   "addresses [id] on [networkId]",
		Short: "Fetch all addresses allocated to node [id] on a given network",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 3 {
				log.Fatalf("%v requires 2 arguments", c.UseLine())
			}
			node := &api.Node{}
			net := &api.Network{}
			if err := session.Fetch(node, args[0]); err != nil {
				log.Fatalln("Failed to fetch node:", err)
			}
			if err := session.Fetch(net, args[2]); err != nil {
				log.Fatalln("Failed to fetch network:", err)
			}
			allocations := []*api.NetworkAllocation{}
			matcher := make(map[string]interface{})
			matcher["network_id"] = net.ID
			matcher["node_id"] = node.ID
			if err := session.Match("network_allocations", matcher, &allocations); err != nil {
				log.Fatalln("Failed to fetch allocations:", err)
			}
			addresses := make([]string, len(allocations))
			for i := range allocations {
				addresses[i] = allocations[i].Address
			}
			type result struct {
				Node      string   `json:"node"`
				Network   string   `json:"network"`
				Category  string   `json:"category"`
				Addresses []string `json:"addresses"`
			}
			res := result{Node: node.Name, Network: net.Name, Category: net.Category, Addresses: addresses}
			fmt.Println(prettyJSON(res))
		},
	})
	nodes.AddCommand(&cobra.Command{
		Use:   "bind [id] to [roleId]",
		Short: "Bind a node to a role",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 3 || args[1] != "to" {
				log.Fatalf("%v requires 2 arguments seperated by \"to\"", c.UseLine())
			}
			obj := &api.Node{}

			role := &api.Role{}
			if err := session.Fetch(obj, args[0]); err != nil {
				log.Fatalf("Failed to fetch Node: %v\n", err.Error())
			}
			if err := session.Fetch(role, args[2]); err != nil {
				log.Fatalf("Failed to fetch Role: %v\n", err.Error())
			}
			nr := &api.NodeRole{}
			if err := session.Init(nr); err != nil {
				log.Fatalf("Failed to initialize NodeRole with defaults\n%v\n", err)
			}
			nr.RoleID = role.ID
			nr.NodeID = obj.ID
			nr.DeploymentID = obj.DeploymentID
			if err := session.BaseCreate(nr); err != nil {
				log.Fatalf("Failed to create noderole for node:%v role:%v\n", args[0], args[2])
			}
			fmt.Println(prettyJSON(nr))
		},
	})
	nodes.AddCommand(&cobra.Command{
		Use:   "import [json]",
		Short: "Import a node definition as a JSON blob.  May include a mac and an IP hint.",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := &api.Node{}
			if err := session.Import(obj, []byte(args[0])); err != nil {
				log.Fatalf("Failed to create node\n%v\n", err)
			}
			fmt.Println(prettyJSON(obj))
		},
	})
	nodes.AddCommand(&cobra.Command{
		Use:   "move [id] to [deployment]",
		Short: "Atomically move a node from one deployment to another",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 3 {
				log.Fatalf("%v requires 2 argument\n", c.UseLine())
			}
			obj := &api.Node{}
			if session.Fetch(obj, args[0]) != nil {
				log.Fatalf("Failed to fetch %v\n", singularName)
			}
			depl := &api.Deployment{}
			if session.Fetch(depl, args[2]) != nil {
				log.Fatalf("Failed fetch deployment")
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
			obj := &api.Node{}
			if session.SetId(obj, args[0]) != nil {
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
			obj := &api.Node{}
			if session.SetId(obj, args[0]) != nil {
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
			obj := &api.Node{}
			if session.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			fmt.Println(obj.ActiveBootstate())
		},
	})
	nodes.AddCommand(&cobra.Command{
		Use:   "redeploy [id]",
		Short: "Force a node to redeploy itself from scratch",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires one argument\n", c.UseLine())
			}
			obj := &api.Node{}
			if session.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for a Node\n", args[0])
			}
			if err := obj.Redeploy(); err != nil {
				log.Fatalf("Failed to redeploy %v\n%v\n", args[0], err)
			}
			fmt.Println(prettyJSON(obj))
		},
	})
	nodes.AddCommand(&cobra.Command{
		Use:   "scrub [id]",
		Short: "Scrub all extraneous noderoles from a node",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires one argument\n", c.UseLine())
			}
			obj := &api.Node{}
			if session.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for a Node\n", args[0])
			}
			if err := obj.Scrub(); err != nil {
				log.Fatalf("Failed to scrub %v\n%v\n", args[0], err)
			}
			fmt.Println(prettyJSON(obj))
		},
	})
	app.AddCommand(nodes)
}
