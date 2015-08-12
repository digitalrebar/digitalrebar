package main

import (
	"encoding/json"
	"fmt"
	"log"

	crowbar "github.com/VictorLowther/crowbar-api"
	"github.com/VictorLowther/jsonpatch/utils"
	"github.com/guregu/null"
	"github.com/spf13/cobra"
)

func addNetworkerCommands(singularName string,
	maker func() crowbar.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(crowbar.Networker); !ok {
		return
	}
	cmd := &cobra.Command{
		Use:   "networks [id]",
		Short: fmt.Sprintf("List all networks for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(crowbar.Networker)
			if crowbar.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			objs, err := crowbar.Networks(obj)
			if err != nil {
				log.Fatalf("Failed to get networks for %v(%v)\n", singularName, args[0])
			}
			fmt.Println(prettyJSON(objs))
		},
	}
	res.AddCommand(cmd)
}

func addNetworkRangerCommands(singularName string,
	maker func() crowbar.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(crowbar.NetworkRanger); !ok {
		return
	}
	cmd := &cobra.Command{
		Use:   "networkranges [id]",
		Short: fmt.Sprintf("List all networkRanges for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(crowbar.NetworkRanger)
			if crowbar.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			objs, err := crowbar.NetworkRanges(obj)
			if err != nil {
				log.Fatalf("Failed to get networkRanges for %v(%v)\n", singularName, args[0])
			}
			fmt.Println(prettyJSON(objs))
		},
	}
	res.AddCommand(cmd)
}

func addNetworkAllocaterCommands(singularName string,
	maker func() crowbar.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(crowbar.NetworkAllocater); !ok {
		return
	}
	cmd := &cobra.Command{
		Use:   "networkallocations [id]",
		Short: fmt.Sprintf("List all networkallocations for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(crowbar.NetworkAllocater)
			if crowbar.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			objs, err := crowbar.NetworkAllocations(obj)
			if err != nil {
				log.Fatalf("Failed to get networkallocations for %v(%v)\n", singularName, args[0])
			}
			fmt.Println(prettyJSON(objs))
		},
	}
	res.AddCommand(cmd)
}

func addNetworkRouterCommands(singularName string,
	maker func() crowbar.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(crowbar.NetworkRouterer); !ok {
		return
	}
	cmd := &cobra.Command{
		Use:   "networkrouters [id]",
		Short: fmt.Sprintf("List all networkrouters for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(crowbar.NetworkRouterer)
			if crowbar.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			objs, err := crowbar.NetworkRouters(obj)
			if err != nil {
				log.Fatalf("Failed to get networkrouters for %v(%v)\n", singularName, args[0])
			}
			fmt.Println(prettyJSON(objs))
		},
	}
	res.AddCommand(cmd)
}

func init() {
	maker := func() crowbar.Crudder { return &crowbar.Network{} }
	network := makeCommandTree("network", maker)
	cmds := []*cobra.Command{
		&cobra.Command{
			Use:   "import [json]",
			Short: "Import a network + optional ranges and routers into Crowbar",
			Run: func(c *cobra.Command, args []string) {
				if len(args) != 1 {
					log.Fatalf("%v requires 1 argument\n", c.UseLine())
				}
				obj := &crowbar.Network{}
				if err := crowbar.Init(obj); err != nil {
					log.Fatalf("Unable to fetch defaults for network\n%v\n", err)
				}
				intermediate := map[string]interface{}{}
				if err := utils.Remarshal(obj, intermediate); err != nil {
					log.Fatalf("Should not happen in network import")
				}
				netdef := map[string]interface{}{}
				if err := json.Unmarshal([]byte(args[0]), &netdef); err != nil {
					log.Fatalf("Argument does not contain a valid network!\n")
				}
				netdef = utils.Merge(intermediate, netdef).(map[string]interface{})
				if v, ok := netdef["deployment"]; ok {
					depl_name, ok := v.(string)
					if !ok {
						log.Fatalln("deployment parameter not a string")
					}
					depl := &crowbar.Deployment{}
					depl.SetId(depl_name)
					if err := crowbar.Read(depl); err != nil {
						log.Fatalf("Unable to fetch deployment %v\n", depl_name)
					}
					netdef["deployment_id"] = depl.ID
				}
				if err := utils.Remarshal(netdef, obj); err != nil {
					log.Fatalln("SHould not happen 2 in networks import")
				}

				if err := crowbar.Create(obj); err != nil {
					log.Fatalln("Failed to create new network.")
				}
				unwind := true
				toClean := []crowbar.Crudder{obj}
				defer func() {
					if unwind {
						for _, o := range toClean {
							crowbar.Destroy(o)
						}
					}
				}()

				type rangeHelper struct {
					Ranges []*crowbar.NetworkRange `json:"ranges"`
				}

				ranges := &rangeHelper{}
				if err := json.Unmarshal([]byte(args[0]), ranges); err != nil {
					log.Fatalln("Failed to unmarshal Ranges")
				}
				if ranges != nil {
					for _, netRange := range ranges.Ranges {
						netRange.NetworkID = obj.ID
						if err := crowbar.Create(netRange); err != nil {
							log.Fatalln("Failed to create network range")
						}
						toClean = append(toClean, netRange)
					}
				}

				type routerHelper struct {
					Router *crowbar.NetworkRouter `json:"router"`
				}
				router := &routerHelper{}
				if err := json.Unmarshal([]byte(args[0]), router); err != nil {
					log.Fatalln("Failed to unmarshal Routers")
				}
				if router != nil && router.Router != nil {
					router.Router.NetworkID = obj.ID
					if err := crowbar.Create(router.Router); err != nil {
						log.Fatalln("Failed to create network router")
					}
				}
				unwind = false
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
				node := &crowbar.Node{}
				network := &crowbar.Network{}
				if err := crowbar.SetId(node, args[0]); err != nil {
					log.Fatalf("Unable to use %v as a node ID", args[0])
				}
				if err := crowbar.SetId(network, args[2]); err != nil {
					log.Fatalf("Unable to use %v as a network ID", args[2])
				}
				if err := crowbar.Read(node); err != nil {
					log.Fatalln("Unable to fetch node from Crowbar")
				}
				if err := crowbar.Read(network); err != nil {
					log.Fatalln("Unable to fetch network from Crowbar")
				}
				alloc := &crowbar.NetworkAllocation{}
				alloc.NodeID = null.IntFrom(node.ID)
				alloc.NetworkID = network.ID
				if err := crowbar.Create(alloc); err != nil {
					log.Fatalf("Unable to create new NetworkAllocation: %v", err)
				}
				fmt.Println(prettyJSON(alloc))
			},
		},
		&cobra.Command{
			Use:   "alloc [id] on [nodeId] from [rangeId] hint [address]",
			Short: "Add a Network to a Node with an address allocated from a specific Range.",
			Run: func(c *cobra.Command, args []string) {
				if len(args) != 5 && len(args) != 7 {
					log.Fatalf("%v requires 3 or 4 arguments", c.UseLine())
				}
				node := &crowbar.Node{}
				if err := crowbar.SetId(node, args[0]); err != nil {
					log.Fatalf("Unable to use %v as a node ID", args[0])
				}
				if err := crowbar.Read(node); err != nil {
					log.Fatalln("Unable to fetch node from Crowbar")
				}
				netRange := &crowbar.NetworkRange{}
				if err := crowbar.SetId(netRange, args[4]); err != nil {
					vals := map[string]interface{}{}
					vals["name"] = args[4]
					network := &crowbar.Network{}
					if err := crowbar.SetId(network, args[2]); err != nil {
						log.Fatalf("Unable to use %v as a network ID", args[2])
					}
					if err := crowbar.Read(network); err != nil {
						log.Fatalln("Unable to fetch network from Crowbar")
					}
					vals["network_id"] = network.ID
					netRanges := []*crowbar.NetworkRange{}
					if err := crowbar.Match(netRange.ApiName(), vals, &netRanges); err != nil {
						log.Fatalf("Unable to fetch ranges matching %#v\n%v\n", netRange, err)
					}
					if len(netRanges) != 1 {
						log.Fatalln("Supplied network range information is ambiguous")
					}
					netRange = netRanges[0]
				} else {
					if err := crowbar.Read(netRange); err != nil {
						log.Fatalln("Unable to fetch network range")
					}
				}
				alloc := &crowbar.NetworkAllocation{}
				alloc.NetworkID = netRange.NetworkID
				alloc.NetworkRangeID = netRange.ID
				alloc.NodeID = null.IntFrom(node.ID)
				if len(args) == 7 {
					alloc.Address = args[6]
				}
				if err := crowbar.Create(alloc); err != nil {
					log.Fatalf("Unable to add node to network\n%v\n", err)
				}
				fmt.Println(prettyJSON(alloc))
			},
		},
	}
	network.AddCommand(cmds...)
	app.AddCommand(network)
	maker = func() crowbar.Crudder { return &crowbar.NetworkRange{} }
	networkrange := makeCommandTree("networkrange", maker)
	app.AddCommand(networkrange)
	maker = func() crowbar.Crudder { return &crowbar.NetworkAllocation{} }
	app.AddCommand(makeCommandTree("networkallocation", maker))
	maker = func() crowbar.Crudder { return &crowbar.NetworkRouter{} }
	app.AddCommand(makeCommandTree("networkrouter", maker))
}
