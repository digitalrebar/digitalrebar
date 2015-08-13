package main

import "github.com/VictorLowther/crowbar-api/client"

func init() {
	maker := func() client.Crudder { return &client.NodeRole{} }
	singularName := "noderole"
	app.AddCommand(makeCommandTree(singularName, maker))
}
