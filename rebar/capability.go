package main

import "github.com/digitalrebar/rebar-api/client"

func init() {
	app.AddCommand(makeCommandTree("capability",
		func() client.Crudder { return &client.Capability{} },
	))
}
