package main

import "github.com/digitalrebar/rebar-api/api"

func init() {
	app.AddCommand(makeCommandTree("tenant",
		func() api.Crudder { return &api.Tenant{} },
	))
}
