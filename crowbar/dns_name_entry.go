package main

import "github.com/VictorLowther/crowbar-api/client"

func init() {
	maker := func() client.Crudder { return &client.DnsNameEntry{} }
	singularName := "dnsnameentry"
	app.AddCommand(makeCommandTree(singularName, maker))
}
