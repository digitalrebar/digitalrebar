package api

import (
	"github.com/rackn/digitalrebar/go/rebar-api/datatypes"
)

type Event struct {
	datatypes.Event
	Timestamps
	apiHelper
}

type EventSink struct {
	datatypes.EventSink
	Timestamps
	apiHelper
}

type EventSelector struct {
	datatypes.EventSelector
	Timestamps
	apiHelper
}
