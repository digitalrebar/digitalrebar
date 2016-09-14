package client

// Deprecated: use api instead. client will not be updated

import "github.com/rackn/digitalrebar/go/rebar-api/datatypes"

// DnsNameFilter wraps datatypes.DnsNameFilter to provide client API
// functionality.
type DnsNameFilter struct {
	datatypes.DnsNameFilter
	Timestamps
	apiHelper
}

// DnsNameFilters lists all the DNS name filters in the system.
func DnsNameFilters() (res []*DnsNameFilter, err error) {
	res = make([]*DnsNameFilter, 0)
	return res, List("dns_name_filters", &res)
}
