package main

import (
	crowbar "github.com/VictorLowther/crowbar-api"
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
	singularName := "barclamp"
	app.AddCommand(makeCommandTree(singularName, lister, maker))
}
