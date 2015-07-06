package main

import (
	"encoding/json"
	"fmt"
	crowbar "github.com/VictorLowther/crowbar-api"
	"github.com/spf13/cobra"
	"log"
)

func addAttriberCommands(singularName string,
	maker func() crowbar.Crudder,
	res *cobra.Command) {
	if _, ok := maker().(crowbar.Attriber); !ok {
		return
	}
	commands := make([]*cobra.Command, 5)
	commands[0] = &cobra.Command{
		Use:   "attribs [id]",
		Short: fmt.Sprintf("List all attribs for a specifc %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(crowbar.Attriber)
			if crowbar.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			objs, err := crowbar.Attribs(obj)
			if err != nil {
				log.Fatalf("Failed to get attribs for %v(%v)\n", singularName, args[0])
			}
			fmt.Println(prettyJSON(objs))
		},
	}
	commands[1] = &cobra.Command{
		Use:   "get [id] attrib [attrib] bucket [bucket=all]",
		Short: fmt.Sprintf("Get an attrib for a specific %v in a specific bucket.", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 3 && len(args) != 5 {
				log.Fatalf("%v requires 2 or 3 arguments.\n", c.UseLine())
			}
			obj := maker().(crowbar.Attriber)
			attr := &crowbar.Attrib{}
			if crowbar.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			if crowbar.SetId(attr, args[2]) != nil {
				log.Fatalf("Failed to parse ID %v for an attrib\n", args[1])
			}
			bucket := "all"
			if len(args) == 5 {
				bucket = args[4]
			}
			attr, err := crowbar.GetAttrib(obj, attr, bucket)
			if err != nil {
				log.Fatalf("Error fetching attrib %v for %v(%v): %v\n", args[1], singularName, args[0], err.Error())
			}
			fmt.Println(prettyJSON(attr))
		},
	}
	commands[2] = &cobra.Command{
		Use:   "set [id] attrib [attrib] to [json]",
		Short: fmt.Sprintf("Set an attrib value for a specific %v", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 5 {
				log.Fatalf("%v requires 3 arguments.\n", c.UseLine())
			}
			obj := maker().(crowbar.Attriber)
			attr := &crowbar.Attrib{}
			if crowbar.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			if crowbar.SetId(attr, args[1]) != nil {
				log.Fatalf("Failed to parse ID %v for an attrib\n", args[1])
			}
			type Valuer struct {
				Value interface{} `json:"value"`
			}
			val := &Valuer{}
			err := json.Unmarshal([]byte(args[2]), val)
			if err != nil {
				log.Fatalf("Failed to parse %v as JSON!\nError: %v\n", args[2], err.Error())
			}
			attr.Value = val.Value
			if err := crowbar.SetAttrib(obj, attr); err != nil {
				log.Fatalf("Unable to set attrib! Error: %v\n", err.Error())
			}
			fmt.Println(prettyJSON(attr))
		},
	}
	commands[3] = &cobra.Command{
		Use:   "propose [id]",
		Short: fmt.Sprintf("Tell a specific %v to prepare to accept new attrib values", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(crowbar.Attriber)
			if crowbar.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			if err := crowbar.Propose(obj); err != nil {
				log.Fatalf("Failed to propose: %v\n", err.Error())
			}
			fmt.Println(prettyJSON(obj))
		},
	}
	commands[4] = &cobra.Command{
		Use:   "commit [id]",
		Short: fmt.Sprintf("Have %v commit any new attrib values", singularName),
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires 1 argument\n", c.UseLine())
			}
			obj := maker().(crowbar.Attriber)
			if crowbar.SetId(obj, args[0]) != nil {
				log.Fatalf("Failed to parse ID %v for an %v\n", args[0], singularName)
			}
			if err := crowbar.Commit(obj); err != nil {
				log.Fatalf("Failed to commit: %v\n", err.Error())
			}
			fmt.Println(prettyJSON(obj))
		},
	}
	res.AddCommand(commands...)
}

func init() {
	lister := func() ([]crowbar.Crudder, error) {
		attribs, err := crowbar.Attribs()
		if err != nil {
			return nil, err
		}
		res := make([]crowbar.Crudder, len(attribs))
		for i := range attribs {
			res[i] = attribs[i]
		}
		return res, nil
	}
	maker := func() crowbar.Crudder { return &crowbar.Attrib{} }
	matcher := func(sample string) (string, error) {
		obj := &crowbar.Attrib{}
		err := json.Unmarshal([]byte(sample), obj)
		if err != nil {
			return "", fmt.Errorf("Error unmarshalling attrib\nError: %v\n", err.Error())
		}
		objs, err := obj.Match()
		if err != nil {
			return "", fmt.Errorf("Error fetching matches for %v", sample)
		}
		return prettyJSON(objs), nil
	}
	app.AddCommand(makeCommandTree("attrib", lister, matcher, maker))
}
