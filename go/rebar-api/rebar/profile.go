package main

import "github.com/digitalrebar/digitalrebar/go/rebar-api/api"

func init() {
	maker := func() api.Crudder { return &api.Profile{} }
	singularName := "profile"
	app.AddCommand(makeCommandTree(singularName, maker))
}
