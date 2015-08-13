package client

import "github.com/VictorLowther/crowbar-api/datatypes"

type DnsNameFilter struct {
	datatypes.DnsNameFilter
	Timestamps
	apiHelper
}

func DnsNameFilters() (res []*DnsNameFilter, err error) {
	res = make([]*DnsNameFilter, 0)
	return res, List("dns_name_filters", &res)
}
