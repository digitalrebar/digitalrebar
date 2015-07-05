package main

import (
	"fmt"
	crowbar "github.com/VictorLowther/crowbar-api"
	"github.com/spf13/cobra"
	"log"
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

func init() {
	lister := func() ([]crowbar.Crudder, error) {
		objs, err := crowbar.Networks()
		if err != nil {
			return nil, err
		}
		res := make([]crowbar.Crudder, len(objs))
		for i := range objs {
			res[i] = objs[i]
		}
		return res, nil
	}
	maker := func() crowbar.Crudder { return &crowbar.Network{} }
	network := makeCommandTree("network", lister, maker)

	app.AddCommand(network)
	lister = func() ([]crowbar.Crudder, error) {
		objs, err := crowbar.NetworkRanges()
		if err != nil {
			return nil, err
		}
		res := make([]crowbar.Crudder, len(objs))
		for i := range objs {
			res[i] = objs[i]
		}
		return res, nil
	}
	maker = func() crowbar.Crudder { return &crowbar.NetworkRange{} }
	networkrange := makeCommandTree("networkrange", lister, maker)

	app.AddCommand(networkrange)
	lister = func() ([]crowbar.Crudder, error) {
		objs, err := crowbar.NetworkAllocations()
		if err != nil {
			return nil, err
		}
		res := make([]crowbar.Crudder, len(objs))
		for i := range objs {
			res[i] = objs[i]
		}
		return res, nil
	}
	maker = func() crowbar.Crudder { return &crowbar.NetworkAllocation{} }
	app.AddCommand(makeCommandTree("networkallocation", lister, maker))

}
