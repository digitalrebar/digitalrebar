package main

import "github.com/digitalrebar/rebar-api/api"

func init() {
	app.AddCommand(makeCommandTree("user_tenant_capability",
		func() api.Crudder { return &api.UserTenantCapability{} },
	))
}
