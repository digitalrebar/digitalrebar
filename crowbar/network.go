package main

import (
	"encoding/json"
	"fmt"
	"log"

	crowbar "github.com/VictorLowther/crowbar-api"
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
				log.Fatalf("Failed to get networks for %v(%v)\n%v\n", singularName, args[0], err)
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
			if err := crowbar.SetId(obj, args[0]); err != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n%v\n", args[0], singularName, err)
			}
			objs, err := crowbar.NetworkRanges(obj)
			if err != nil {
				log.Fatalf("Failed to get networkRanges for %v(%v)\n%v\n", singularName, args[0], err)
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
			if err := crowbar.SetId(obj, args[0]); err != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n%v\n", args[0], singularName, err)
			}
			objs, err := crowbar.NetworkAllocations(obj)
			if err != nil {
				log.Fatalf("Failed to get networkallocations for %v(%v)\n%v\n", singularName, args[0], err)
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
			if err := crowbar.SetId(obj, args[0]); err != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n%v\n", args[0], singularName, err)
			}
			objs, err := crowbar.NetworkRouters(obj)
			if err != nil {
				log.Fatalf("Failed to get networkrouters for %v(%v)\n%v\n", singularName, args[0], err)
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
				netdef := map[string]interface{}{}
				if err := json.Unmarshal([]byte(args[0]), &netdef); err != nil {
					log.Fatalf("Argument does not contain a valid network!\n%v\n", err)
				}
				if v, ok := netdef["deployment"]; ok {
					depl_name, ok := v.(string)
					if !ok {
						log.Fatalln("deployment parameter not a string")
					}
					depl := &crowbar.Deployment{}
					depl.SetId(depl_name)
					if err := crowbar.Read(depl); err != nil {
						log.Fatalf("Unable to fetch deployment %v\n%v\n", depl_name, err)
					}
					netdef["deployment_id"] = depl.ID
					delete(netdef, "deployment")
				}
				buf, err := json.Marshal(netdef)
				if err != nil {
					log.Fatalln("Failed to marshal fixed-up network definition", err)
				}

				if err := crowbar.CreateJSON(obj, buf); err != nil {
					log.Fatalln("Failed to create new network.\n%v\n", err)
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
					Ranges []interface{} `json:"ranges"`
				}

				ranges := &rangeHelper{}
				if err := json.Unmarshal([]byte(args[0]), ranges); err != nil {
					log.Fatalln("Failed to unmarshal Ranges")
				}
				if ranges != nil {
					for _, netRange := range ranges.Ranges {
						rangeObj := &crowbar.NetworkRange{}
						if err := crowbar.Init(rangeObj); err != nil {
							log.Fatalf("Failed to initialize new NetworkRange\n%v\n", err)
						}
						rangeObj.NetworkID = obj.ID
						if err := crowbar.Create(rangeObj, netRange); err != nil {
							log.Fatalln("Failed to create network range\n%v\n", err)
						}
						toClean = append(toClean, rangeObj)
					}
				}

				type routerHelper struct {
					Router interface{} `json:"router"`
				}
				router := &routerHelper{}
				if err := json.Unmarshal([]byte(args[0]), router); err != nil {
					log.Fatalln("Failed to unmarshal Router\n%v\n", err)
				}
				if router != nil && router.Router != nil {
					routerObj := &crowbar.NetworkRouter{}
					routerObj.NetworkID = obj.ID
					if err := crowbar.Create(routerObj, router.Router); err != nil {
						log.Fatalln("Failed to create network router", err)
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
				if err := crowbar.SetId(node, args[2]); err != nil {
					log.Fatalf("Unable to use %v as a node ID\n%v\n", args[2], err)
				}
				if err := crowbar.SetId(network, args[0]); err != nil {
					log.Fatalf("Unable to use %v as a network ID\n%v\n", args[0], err)
				}
				if err := crowbar.Read(node); err != nil {
					log.Fatalln("Unable to fetch node from Crowbar", err)
				}
				if err := crowbar.Read(network); err != nil {
					log.Fatalln("Unable to fetch network from Crowbar", err)
				}
				ranges, err := network.AutoRanges(node)
				if err != nil {
					log.Fatalln("Failed to get auto ranges for network", err)
				}
				res := make([]*crowbar.NetworkAllocation, len(ranges))
				unwind := true
				defer func() {
					if unwind {
						for _, allocation := range res {
							if allocation != nil {
								crowbar.Destroy(allocation)
							}
						}
					}
				}()

				for i, netRange := range ranges {
					alloc := &crowbar.NetworkAllocation{}
					alloc.NodeID = null.IntFrom(node.ID)
					alloc.NetworkID = null.IntFrom(network.ID)
					alloc.NetworkRangeID = null.IntFrom(netRange.ID)

					if err := crowbar.BaseCreate(alloc); err != nil {
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
				node := &crowbar.Node{}
				if err := crowbar.SetId(node, args[0]); err != nil {
					log.Fatalf("Unable to use %v as a node ID\n%v\n", args[0], err)
				}
				if err := crowbar.Read(node); err != nil {
					log.Fatalln("Unable to fetch node from Crowbar\n%v\n", err)
				}
				netRange := &crowbar.NetworkRange{}
				if err := crowbar.SetId(netRange, args[4]); err != nil {
					vals := map[string]interface{}{}
					vals["name"] = args[4]
					network := &crowbar.Network{}
					if err := crowbar.SetId(network, args[2]); err != nil {
						log.Fatalf("Unable to use %v as a network ID\n%v\n", args[2], err)
					}
					if err := crowbar.Read(network); err != nil {
						log.Fatalln("Unable to fetch network from Crowbar\n%v\n", err)
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
						log.Fatalln("Unable to fetch network range\n%v\n", err)
					}
				}
				alloc := &crowbar.NetworkAllocation{}
				alloc.NetworkID = null.IntFrom(netRange.NetworkID)
				alloc.NetworkRangeID = null.IntFrom(netRange.ID)
				alloc.NodeID = null.IntFrom(node.ID)
				if len(args) == 7 {
					alloc.Address = args[6]
				}
				if err := crowbar.BaseCreate(alloc); err != nil {
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
