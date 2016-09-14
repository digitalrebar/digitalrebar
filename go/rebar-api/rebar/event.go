package main

import "github.com/digitalrebar/rebar-api/api"

func init() {
	app.AddCommand(makeCommandTree("event",
		func() api.Crudder { return &api.Event{} },
	))
	app.AddCommand(makeCommandTree("eventsink",
		func() api.Crudder { return &api.EventSink{} },
	))
	app.AddCommand(makeCommandTree("eventselector",
		func() api.Crudder { return &api.EventSelector{} },
	))
}
