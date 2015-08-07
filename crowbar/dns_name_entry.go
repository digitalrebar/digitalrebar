package main

import crowbar "github.com/VictorLowther/crowbar-api"

func init() {
	maker := func() crowbar.Crudder { return &crowbar.DnsNameEntry{} }
	singularName := "dnsnameentry"
	app.AddCommand(makeCommandTree(singularName, maker))
}
