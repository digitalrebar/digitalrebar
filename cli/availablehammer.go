package main

import (
	"log"
	"encoding/json"
	"fmt"

	crowbar "github.com/VictorLowther/crowbar-api"
)

func init() {
	lister := func() ([]crowbar.Crudder, error) {
		availableHammers, err := crowbar.AvailableHammers()
		if err != nil {
			return nil, err
		}
		res := make([]crowbar.Crudder, len(availableHammers))
		for i := range availableHammers {
			res[i] = availableHammers[i]
		}
		return res, nil
	}
	maker := func() crowbar.Crudder { return &crowbar.AvailableHammer{} }
	singularName := "availablehammer"
	matcher := func(sample string) (string, error) {
		obj := &crowbar.AvailableHammer{}
		err := json.Unmarshal([]byte(sample), obj)
		if err != nil {
			return "", fmt.Errorf("Error unmarshalling availablehammer\nError: %v\n", err.Error())
		}
		objs, err := obj.Match()
		if err != nil {
			return "", fmt.Errorf("Error fetching matches for %v", sample)
		}
		log.Printf("%#v",objs)
		return prettyJSON(objs), nil
	}
	app.AddCommand(makeCommandTree(singularName, lister, matcher, maker))
}
