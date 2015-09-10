package client

import "github.com/digitalrebar/rebar-api/datatypes"

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
