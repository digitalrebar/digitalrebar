package main

import (
	"encoding/json"
	"fmt"

	crowbar "github.com/VictorLowther/crowbar-api"
	//	"github.com/spf13/cobra"
	//	"log"
)

func init() {
	lister := func() ([]crowbar.Crudder, error) {
		barclamps, err := crowbar.Barclamps()
		if err != nil {
			return nil, err
		}
		res := make([]crowbar.Crudder, len(barclamps))
		for i := range barclamps {
			res[i] = barclamps[i]
		}
		return res, nil
	}
	maker := func() crowbar.Crudder { return &crowbar.Barclamp{} }
	matcher := func(sample string) (string, error) {
		obj := &crowbar.Barclamp{}
		err := json.Unmarshal([]byte(sample), obj)
		if err != nil {
			return "", fmt.Errorf("Error unmarshalling barclamp\nError: %v\n", err.Error())
		}
		objs, err := obj.Match()
		if err != nil {
			return "", fmt.Errorf("Error fetching matches for %v", sample)
		}
		return prettyJSON(objs), nil
	}
	singularName := "barclamp"
	barclamps := makeCommandTree(singularName, lister, matcher, maker)
	/* Not actaully implemented yet
	barclamps.AddCommand(&cobra.Command{
		Use: "import [path]",
		Short: "Import a barclamp from [path]",
		Run: func(c *cobra.Command, args []string) {
			if len(args) != 1 {
				log.Fatalf("%v requires exactly 1 argument\n",c.UseLine())
			}
			res, err := crowbar.BarclampImport(args[0])
			if err != nil {
				log.Fatalf("Error importing barclamp from %v!\n Error: %v\n",args[0], err.Error())
			}
			fmt.Println(prettyJSON(res))
		},
	}) */
	app.AddCommand(barclamps)
}
