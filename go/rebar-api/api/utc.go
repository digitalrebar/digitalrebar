package api

import (
	"github.com/digitalrebar/rebar-api/datatypes"
)

type UserTenantCapability struct {
	datatypes.UserTenantCapability
	Timestamps
	apiHelper
}
