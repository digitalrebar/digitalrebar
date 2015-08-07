package main

import crowbar "github.com/VictorLowther/crowbar-api"

func init() {
	maker := func() crowbar.Crudder { return &crowbar.DnsNameFilter{} }
	singularName := "dnsnamefilter"
	app.AddCommand(makeCommandTree(singularName, maker))
}
