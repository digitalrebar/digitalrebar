package api

import "github.com/digitalrebar/digitalrebar/go/rebar-api/datatypes"

// DnsNameFilter wraps datatypes.DnsNameFilter to provide client API
// functionality.
type DnsNameFilter struct {
	datatypes.DnsNameFilter
	Timestamps
	apiHelper
	rebarSrc
}

// DnsNameFilters lists all the DNS name filters in the system.
func (c *Client) DnsNameFilters() (res []*DnsNameFilter, err error) {
	res = make([]*DnsNameFilter, 0)
	dnf := &DnsNameFilter{}
	return res, c.List(c.UrlPath(dnf), &res)
}
