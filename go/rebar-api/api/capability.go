package api

import (
	"github.com/rackn/digitalrebar/go/rebar-api/datatypes"
)

type Capability struct {
	datatypes.Capability
	Timestamps
	apiHelper
}
