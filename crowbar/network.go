package main

import (
	"fmt"
	"log"

	"github.com/VictorLowther/crowbar-api/client"
	"github.com/guregu/null"
	"github.com/spf13/cobra"
)

func addNetworkerCommands(singularName string,
	maker func() client.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(client.Networker); !ok {
		return
	}
	cmd := &cobra.Command{
		Use:   "networks [id]",
		Short: fmt.Sprintf("List all networks for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(client.Networker)
			if client.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			objs, err := client.Networks(obj)
			if err != nil {
				log.Fatalf("Failed to get networks for %v(%v)\n%v\n", singularName, args[0], err)
			}
			fmt.Println(prettyJSON(objs))
		},
	}
	res.AddCommand(cmd)
}

func addNetworkRangerCommands(singularName string,
	maker func() client.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(client.NetworkRanger); !ok {
		return
	}
	cmd := &cobra.Command{
		Use:   "networkranges [id]",
		Short: fmt.Sprintf("List all networkRanges for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(client.NetworkRanger)
			if err := client.SetId(obj, args[0]); err != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n%v\n", args[0], singularName, err)
			}
			objs, err := client.NetworkRanges(obj)
			if err != nil {
				log.Fatalf("Failed to get networkRanges for %v(%v)\n%v\n", singularName, args[0], err)
			}
			fmt.Println(prettyJSON(objs))
		},
	}
	res.AddCommand(cmd)
}

func addNetworkAllocaterCommands(singularName string,
	maker func() client.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(client.NetworkAllocater); !ok {
		return
	}
	cmd := &cobra.Command{
		Use:   "networkallocations [id]",
		Short: fmt.Sprintf("List all networkallocations for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(client.NetworkAllocater)
			if err := client.SetId(obj, args[0]); err != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n%v\n", args[0], singularName, err)
			}
			objs, err := client.NetworkAllocations(obj)
			if err != nil {
				log.Fatalf("Failed to get networkallocations for %v(%v)\n%v\n", singularName, args[0], err)
			}
			fmt.Println(prettyJSON(objs))
		},
	}
	res.AddCommand(cmd)
}

func addNetworkRouterCommands(singularName string,
	maker func() client.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(client.NetworkRouterer); !ok {
		return
	}
	cmd := &cobra.Command{
		Use:   "networkrouters [id]",
		Short: fmt.Sprintf("List all networkrouters for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(client.NetworkRouterer)
			if err := client.SetId(obj, args[0]); err != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n%v\n", args[0], singularName, err)
			}
			objs, err := client.NetworkRouters(obj)
			if err != nil {
				log.Fatalf("Failed to get networkrouters for %v(%v)\n%v\n", singularName, args[0], err)
			}
			fmt.Println(prettyJSON(objs))
		},
	}
	res.AddCommand(cmd)
}

func init() {
	maker := func() client.Crudder { return &client.Network{} }
	network := makeCommandTree("network", maker)
	cmds := []*cobra.Command{
		&cobra.Command{
			Use:   "import [json]",
			Short: "Import a network + optional ranges and routers into Crowbar",
			Run: func(c *cobra.Command, args []string) {
				if len(args) != 1 {
					log.Fatalf("%v requires 1 argument\n", c.UseLine())
				}
				obj := &client.Network{}
				if err := client.Import(obj, []byte(args[0])); err != nil {
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
				node := &client.Node{}
				network := &client.Network{}
				if err := client.Fetch(node, args[2]); err != nil {
					log.Fatalf("Unable to fetch node\n%v\n", err)
				}
				if err := client.Fetch(network, args[0]); err != nil {
					log.Fatalf("Unable to fetch network\n%v\n", err)
				}
				ranges, err := network.AutoRanges(node)
				if err != nil {
					log.Fatalln("Failed to get auto ranges for network", err)
				}
				res := make([]*client.NetworkAllocation, len(ranges))
				unwind := true
				defer func() {
					if unwind {
						for _, allocation := range res {
							if allocation != nil {
								client.Destroy(allocation)
							}
						}
					}
				}()

				for i, netRange := range ranges {
					alloc := &client.NetworkAllocation{}
					alloc.NodeID = null.IntFrom(node.ID)
					alloc.NetworkID = null.IntFrom(network.ID)
					alloc.NetworkRangeID = null.IntFrom(netRange.ID)

					if err := client.BaseCreate(alloc); err != nil {
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
				node := &client.Node{}
				if err := client.Fetch(node, args[0]); err != nil {
					log.Fatalln("Unable to fetch node from Crowbar\n%v\n", err)
				}
				netRange := &client.NetworkRange{}
				if err := client.SetId(netRange, args[4]); err != nil {
					vals := map[string]interface{}{}
					vals["name"] = args[4]
					network := &client.Network{}
					if err := client.Fetch(network, args[2]); err != nil {
						log.Fatalln("Unable to fetch network from Crowbar\n%v\n", err)
					}
					vals["network_id"] = network.ID
					netRanges := []*client.NetworkRange{}
					if err := client.Match(netRange.ApiName(), vals, &netRanges); err != nil {
						log.Fatalf("Unable to fetch ranges matching %#v\n%v\n", netRange, err)
					}
					if len(netRanges) != 1 {
						log.Fatalln("Supplied network range information is ambiguous")
					}
					netRange = netRanges[0]
				} else {
					if err := client.Read(netRange); err != nil {
						log.Fatalln("Unable to fetch network range\n%v\n", err)
					}
				}
				alloc := &client.NetworkAllocation{}
				alloc.NetworkID = null.IntFrom(netRange.NetworkID)
				alloc.NetworkRangeID = null.IntFrom(netRange.ID)
				alloc.NodeID = null.IntFrom(node.ID)
				if len(args) == 7 {
					alloc.Address = args[6]
				}
				if err := client.BaseCreate(alloc); err != nil {
					log.Fatalf("Unable to add node to network\n%v\n", err)
				}
				fmt.Println(prettyJSON(alloc))
			},
		},
	}
	network.AddCommand(cmds...)
	app.AddCommand(network)
	maker = func() client.Crudder { return &client.NetworkRange{} }
	networkrange := makeCommandTree("networkrange", maker)
	app.AddCommand(networkrange)
	maker = func() client.Crudder { return &client.NetworkAllocation{} }
	app.AddCommand(makeCommandTree("networkallocation", maker))
	maker = func() client.Crudder { return &client.NetworkRouter{} }
	app.AddCommand(makeCommandTree("networkrouter", maker))
}
