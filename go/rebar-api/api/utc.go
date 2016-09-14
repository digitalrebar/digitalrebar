package api

import (
	"github.com/rackn/digitalrebar/go/rebar-api/datatypes"
)

type UserTenantCapability struct {
	datatypes.UserTenantCapability
	Timestamps
	apiHelper
}
