package client

// Deprecated: use api instead. client will not be updated

import (
	"github.com/rackn/digitalrebar/go/rebar-api/datatypes"
)

type Tenant struct {
	datatypes.Tenant
	Timestamps
	apiHelper
}
