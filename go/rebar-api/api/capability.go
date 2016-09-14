package api

import (
	"github.com/digitalrebar/rebar-api/datatypes"
)

type Capability struct {
	datatypes.Capability
	Timestamps
	apiHelper
}
