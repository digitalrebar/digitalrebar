package api

import "github.com/rackn/digitalrebar/go/rebar-api/datatypes"

// DnsNameEntry wraps datatypes.DnsNameEntry to provide client API
// functionality
type DnsNameEntry struct {
	datatypes.DnsNameEntry
	Timestamps
	apiHelper
}

// DnsNameEntrys fetches all of the DnsNameEntrys in Rebar.
func (c *Client) DnsNameEntrys() (res []*DnsNameEntry, err error) {
	res = make([]*DnsNameEntry, 0)
	return res, c.List("dns_name_entries", &res)
}
