package main

import (
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
	app.AddCommand(makeCommandTree(singularName, lister, maker))
}
