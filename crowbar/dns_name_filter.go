package main

import "github.com/VictorLowther/crowbar-api/client"

func init() {
	maker := func() client.Crudder { return &client.DnsNameFilter{} }
	singularName := "dnsnamefilter"
	app.AddCommand(makeCommandTree(singularName, maker))
}
