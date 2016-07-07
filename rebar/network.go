package main

import (
	"fmt"
	"log"

	"github.com/digitalrebar/rebar-api/api"
	"github.com/guregu/null"
	"github.com/spf13/cobra"
)

func addNetworkerCommands(singularName string,
	maker func() api.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(api.Networker); !ok {
		return
	}
	cmd := &cobra.Command{
		Use:   "networks [id]",
		Short: fmt.Sprintf("List all networks for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(api.Networker)
			if session.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			objs, err := session.Networks(obj)
			if err != nil {
				log.Fatalf("Failed to get networks for %v(%v)\n%v\n", singularName, args[0], err)
			}
			fmt.Println(prettyJSON(objs))
		},
	}
	res.AddCommand(cmd)
}

func addNetworkRangerCommands(singularName string,
	maker func() api.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(api.NetworkRanger); !ok {
		return
	}
	cmd := &cobra.Command{
		Use:   "networkranges [id]",
		Short: fmt.Sprintf("List all networkRanges for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(api.NetworkRanger)
			if err := session.SetId(obj, args[0]); err != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n%v\n", args[0], singularName, err)
			}
			objs, err := session.NetworkRanges(obj)
			if err != nil {
				log.Fatalf("Failed to get networkRanges for %v(%v)\n%v\n", singularName, args[0], err)
			}
			fmt.Println(prettyJSON(objs))
		},
	}
	res.AddCommand(cmd)
}

func addNetworkAllocaterCommands(singularName string,
	maker func() api.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(api.NetworkAllocater); !ok {
		return
	}
	cmd := &cobra.Command{
		Use:   "networkallocations [id]",
		Short: fmt.Sprintf("List all networkallocations for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(api.NetworkAllocater)
			if err := session.SetId(obj, args[0]); err != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n%v\n", args[0], singularName, err)
			}
			objs, err := session.NetworkAllocations(obj)
			if err != nil {
				log.Fatalf("Failed to get networkallocations for %v(%v)\n%v\n", singularName, args[0], err)
			}
			fmt.Println(prettyJSON(objs))
		},
	}
	res.AddCommand(cmd)
}

func addNetworkRouterCommands(singularName string,
	maker func() api.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(api.NetworkRouterer); !ok {
		return
	}
	cmd := &cobra.Command{
		Use:   "networkrouters [id]",
		Short: fmt.Sprintf("List all networkrouters for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(api.NetworkRouterer)
			if err := session.SetId(obj, args[0]); err != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n%v\n", args[0], singularName, err)
			}
			objs, err := session.NetworkRouters(obj)
			if err != nil {
				log.Fatalf("Failed to get networkrouters for %v(%v)\n%v\n", singularName, args[0], err)
			}
			fmt.Println(prettyJSON(objs))
		},
	}
	res.AddCommand(cmd)
}

func init() {
	maker := func() api.Crudder { return &api.Network{} }
	network := makeCommandTree("network", maker)
	cmds := []*cobra.Command{
		&cobra.Command{
			Use:   "import [json]",
			Short: "Import a network + optional ranges and routers into Rebar",
			Run: func(c *cobra.Command, args []string) {
				if len(args) != 1 {
					log.Fatalf("%v requires 1 argument\n", c.UseLine())
				}
				obj := &api.Network{}
				if err := session.Import(obj, []byte(args[0])); err != nil {
					log.Fatalf("Unable to import network: %v\n", err)
				}
				fmt.Println(prettyJSON(obj))
			},
		},
		&cobra.Command{
			Use:   "add [id] to [nodeId]",
			Short: "Add a Network to a Node.  This will implicitly allocate an address from the default host ranges in the network.",
			Run: func(c *cobra.Command, args []string) {
				if len(args) != 3 {
					log.Fatalf("%v requires 2 arguments\n", c.UseLine())
				}
				node := &api.Node{}
				network := &api.Network{}
				if err := session.Fetch(node, args[2]); err != nil {
					log.Fatalf("Unable to fetch node\n%v\n", err)
				}
				if err := session.Fetch(network, args[0]); err != nil {
					log.Fatalf("Unable to fetch network\n%v\n", err)
				}
				ranges, err := network.AutoRanges(node)
				if err != nil {
					log.Fatalln("Failed to get auto ranges for network", err)
				}
				res := make([]*api.NetworkAllocation, len(ranges))
				unwind := true
				defer func() {
					if unwind {
						for _, allocation := range res {
							if allocation != nil {
								session.Destroy(allocation)
							}
						}
					}
				}()

				for i, netRange := range ranges {
					alloc := &api.NetworkAllocation{}
					alloc.NodeID = null.IntFrom(node.ID)
					alloc.NetworkID = null.IntFrom(network.ID)
					alloc.NetworkRangeID = null.IntFrom(netRange.ID)

					if err := session.BaseCreate(alloc); err != nil {
						log.Fatalf("Unable to create new NetworkAllocation: %v", err)
					}
					res[i] = alloc
				}
				unwind = false
				fmt.Println(prettyJSON(res))
			},
		},
		&cobra.Command{
			Use:   "alloc [id] on [nodeId] from [rangeId] hint [address]",
			Short: "Add a Network to a Node with an address allocated from a specific Range.",
			Run: func(c *cobra.Command, args []string) {
				if len(args) != 5 && len(args) != 7 {
					log.Fatalf("%v requires 3 or 4 arguments", c.UseLine())
				}
				node := &api.Node{}
				if err := session.Fetch(node, args[0]); err != nil {
					log.Fatalln("Unable to fetch node from Rebar\n%v\n", err)
				}
				netRange := &api.NetworkRange{}
				if err := session.SetId(netRange, args[4]); err != nil {
					vals := map[string]interface{}{}
					vals["name"] = args[4]
					network := &api.Network{}
					if err := session.Fetch(network, args[2]); err != nil {
						log.Fatalln("Unable to fetch network from Rebar\n%v\n", err)
					}
					vals["network_id"] = network.ID
					netRanges := []*api.NetworkRange{}
					if err := session.Match(netRange.ApiName(), vals, &netRanges); err != nil {
						log.Fatalf("Unable to fetch ranges matching %#v\n%v\n", netRange, err)
					}
					if len(netRanges) != 1 {
						log.Fatalln("Supplied network range information is ambiguous")
					}
					netRange = netRanges[0]
				} else {
					if err := session.Read(netRange); err != nil {
						log.Fatalln("Unable to fetch network range\n%v\n", err)
					}
				}
				alloc := &api.NetworkAllocation{}
				alloc.NetworkID = null.IntFrom(netRange.NetworkID)
				alloc.NetworkRangeID = null.IntFrom(netRange.ID)
				alloc.NodeID = null.IntFrom(node.ID)
				if len(args) == 7 {
					alloc.Address = args[6]
				}
				if err := session.BaseCreate(alloc); err != nil {
					log.Fatalf("Unable to add node to network\n%v\n", err)
				}
				fmt.Println(prettyJSON(alloc))
			},
		},
	}
	network.AddCommand(cmds...)
	app.AddCommand(network)
	maker = func() api.Crudder { return &api.NetworkRange{} }
	networkrange := makeCommandTree("networkrange", maker)
	app.AddCommand(networkrange)
	maker = func() api.Crudder { return &api.NetworkAllocation{} }
	app.AddCommand(makeCommandTree("networkallocation", maker))
	maker = func() api.Crudder { return &api.NetworkRouter{} }
	app.AddCommand(makeCommandTree("networkrouter", maker))
}
