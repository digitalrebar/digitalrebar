package main

import crowbar "github.com/VictorLowther/crowbar-api"

func init() {
	maker := func() crowbar.Crudder { return &crowbar.NodeRole{} }
	singularName := "noderole"
	app.AddCommand(makeCommandTree(singularName, maker))
}
