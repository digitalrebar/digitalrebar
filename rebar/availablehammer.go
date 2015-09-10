package main

import "github.com/digitalrebar/rebar-api/client"

func init() {

	maker := func() client.Crudder { return &client.AvailableHammer{} }
	singularName := "availablehammer"
	app.AddCommand(makeCommandTree(singularName, maker))
}
