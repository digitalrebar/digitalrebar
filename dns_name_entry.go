package crowbar

import "github.com/VictorLowther/crowbar-api/datatypes"

type DnsNameEntry struct {
	datatypes.DnsNameEntry
	Timestamps
	apiHelper
}

func DnsNameEntrys() (res []*DnsNameEntry, err error) {
	res = make([]*DnsNameEntry, 0)
	return res, List("dns_name_entries", &res)
}
