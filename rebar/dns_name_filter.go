package main

import "github.com/digitalrebar/rebar-api/client"

func init() {
	maker := func() client.Crudder { return &client.DnsNameFilter{} }
	singularName := "dnsnamefilter"
	app.AddCommand(makeCommandTree(singularName, maker))
}
