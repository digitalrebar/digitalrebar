package api

import (
	"github.com/digitalrebar/digitalrebar/go/rebar-api/datatypes"
)

type UserTenantCapability struct {
	datatypes.UserTenantCapability
	Timestamps
	apiHelper
}
