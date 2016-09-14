package main

import "github.com/rackn/digitalrebar/go/rebar-api/api"

func init() {
	maker := func() api.Crudder { return &api.DnsNameEntry{} }
	singularName := "dnsnameentry"
	app.AddCommand(makeCommandTree(singularName, maker))
}
