package datatypes

import "github.com/guregu/null"

type DnsNameEntry struct {
	NameID
	NetworkAllocationID int64       `json:"network_allocation_id"`
	DnsNameFilterID     int64       `json:"dns_name_filter_id"`
	RRType              null.String `json:"rr_type"`
}

func (o *DnsNameEntry) ApiName() string {
	return "dns_name_entries"
}

type DnsNameFilter struct {
	SimpleID
	Name     string `json:"name"`
	Matcher  string `json:"matcher"`
	Priority int64  `json:"priority"`
	Service  string `json:"service"`
	Template string `json:"template"`
}

func (o *DnsNameFilter) ApiName() string {
	return "dns_name_filters"
}
