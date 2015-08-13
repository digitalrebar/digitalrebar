package main

import "github.com/VictorLowther/crowbar-api/client"

func init() {

	maker := func() client.Crudder { return &client.AvailableHammer{} }
	singularName := "availablehammer"
	app.AddCommand(makeCommandTree(singularName, maker))
}
