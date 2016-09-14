package client

// Deprecated: use api instead. client will not be updated

import "github.com/digitalrebar/digitalrebar/go/rebar-api/datatypes"

// DnsNameEntry wraps datatypes.DnsNameEntry to provide client API
// functionality
type DnsNameEntry struct {
	datatypes.DnsNameEntry
	Timestamps
	apiHelper
}

// DnsNameEntrys fetches all of the DnsNameEntrys in Rebar.
func DnsNameEntrys() (res []*DnsNameEntry, err error) {
	res = make([]*DnsNameEntry, 0)
	return res, List("dns_name_entries", &res)
}
