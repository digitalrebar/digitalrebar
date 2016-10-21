package api

import "github.com/digitalrebar/digitalrebar/go/rebar-api/datatypes"

// Node wraps datatypes.Node to provide the client API.
type Profile struct {
	datatypes.Profile
	Timestamps
	apiHelper
	rebarSrc
}
