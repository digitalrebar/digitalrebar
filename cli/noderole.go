package main

import (
	crowbar "github.com/VictorLowther/crowbar-api"
)

func init() {
	lister := func() ([]crowbar.Crudder, error) {
		noderoles, err := crowbar.NodeRoles()
		if err != nil {
			return nil, err
		}
		res := make([]crowbar.Crudder, len(noderoles))
		for i := range noderoles {
			res[i] = noderoles[i]
		}
		return res, nil
	}
	maker := func() crowbar.Crudder { return &crowbar.NodeRole{} }
	singularName := "noderole"
	app.AddCommand(makeCommandTree(singularName, lister, maker))
}
