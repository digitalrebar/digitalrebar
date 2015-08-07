package main

import crowbar "github.com/VictorLowther/crowbar-api"

func init() {
	maker := func() crowbar.Crudder { return &crowbar.User{} }
	singularName := "user"
	app.AddCommand(makeCommandTree(singularName, maker))
}
