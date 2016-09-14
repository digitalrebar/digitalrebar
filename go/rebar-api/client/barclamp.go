package client

// Deprecated: use api instead. client will not be updated

import "github.com/digitalrebar/digitalrebar/go/rebar-api/datatypes"

// Barclamp wraps datatypes.Barclamp to provide client API
// functionality
type Barclamp struct {
	datatypes.Barclamp
	Timestamps
	apiHelper
}

// Barclamps returns all of the Barclamps.
func Barclamps() (res []*Barclamp, err error) {
	res = make([]*Barclamp, 0)
	return res, List("barclamps", &res)
}
