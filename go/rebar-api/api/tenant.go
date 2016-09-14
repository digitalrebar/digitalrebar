package api

import (
	"github.com/digitalrebar/digitalrebar/go/rebar-api/datatypes"
)

type Tenant struct {
	datatypes.Tenant
	Timestamps
	apiHelper
}
