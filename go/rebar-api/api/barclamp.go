package api

import "github.com/digitalrebar/digitalrebar/go/rebar-api/datatypes"

// Barclamp wraps datatypes.Barclamp to provide client API
// functionality
type Barclamp struct {
	datatypes.Barclamp
	Timestamps
	apiHelper
	rebarSrc
}

// Barclamps returns all of the Barclamps.
func (c *Client) Barclamps() (res []*Barclamp, err error) {
	res = make([]*Barclamp, 0)
	bc := &Barclamp{}
	return res, c.List(c.UrlPath(bc), &res)
}
