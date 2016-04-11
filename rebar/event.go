package main

import "github.com/digitalrebar/rebar-api/client"

func init() {
	app.AddCommand(makeCommandTree("event",
		func() client.Crudder { return &client.Event{} },
	))
	app.AddCommand(makeCommandTree("eventsink",
		func() client.Crudder { return &client.EventSink{} },
	))
	app.AddCommand(makeCommandTree("eventselector",
		func() client.Crudder { return &client.EventSelector{} },
	))
}
