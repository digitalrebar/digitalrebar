package api

import (
	"github.com/rackn/digitalrebar/go/rebar-api/datatypes"
)

type Tenant struct {
	datatypes.Tenant
	Timestamps
	apiHelper
}
