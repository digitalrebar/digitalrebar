package main

import "github.com/digitalrebar/digitalrebar/go/rebar-api/api"

func init() {
	app.AddCommand(makeCommandTree("capability",
		func() api.Crudder { return &api.Capability{} },
	))
}
