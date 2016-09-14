package main

import "github.com/rackn/digitalrebar/go/rebar-api/api"

func init() {

	maker := func() api.Crudder { return &api.AvailableHammer{} }
	singularName := "availablehammer"
	app.AddCommand(makeCommandTree(singularName, maker))
}
