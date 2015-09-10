package main

import "github.com/digitalrebar/rebar-api/client"

func init() {
	maker := func() client.Crudder { return &client.NodeRole{} }
	singularName := "noderole"
	app.AddCommand(makeCommandTree(singularName, maker))
}
