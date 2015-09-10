package main

import "github.com/digitalrebar/rebar-api/client"

func init() {
	maker := func() client.Crudder { return &client.DnsNameEntry{} }
	singularName := "dnsnameentry"
	app.AddCommand(makeCommandTree(singularName, maker))
}
