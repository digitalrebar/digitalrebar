package main

import "github.com/digitalrebar/rebar-api/client"

func init() {
	app.AddCommand(makeCommandTree("tenant",
		func() client.Crudder { return &client.Tenant{} },
	))
}
