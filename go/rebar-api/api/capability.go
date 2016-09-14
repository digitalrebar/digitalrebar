package api

import (
	"github.com/digitalrebar/digitalrebar/go/rebar-api/datatypes"
)

type Capability struct {
	datatypes.Capability
	Timestamps
	apiHelper
}
