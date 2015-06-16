package main

/*
 * Managment API Structures
 *
 * These are the front end api structures.
 * They are similar to the PowerDNS structures, but
 * are different.  The RRSet semantic in PowerDNS
 * is replace/delete.  The management layer is just
 * add/or delete.  The set math is done in this application
 * to figure out the actual resulting set of data to
 * write to the backend for the key (name).
 *
 * These match the json objects that are needed to
 * update/create and get zone information and records
 */
type Zone struct {
	Id               string    `json:"id"`
	Name             string    `json:"name"`
	Url              string    `json:"url"`
	Kind             string    `json:"kind"`
	Type             string    `json:"type,omitempty"`
	Dnssec           bool      `json:"dnssec"`
	Account          string    `json:"account"`
	Serial           int       `json:"serial"`
	NotifiedSerial   int       `json:"notified_serial"`
	Servers          []string  `json:"servers,omitempty"`
	Masters          []string  `json:"masters,omitempty"`
	Nameservers      []string  `json:"nameservers,omitempty"`
	RecursionDesired bool      `json:"recursion_desired,omitempty"`
	LastCheck        int       `json:"last_check"`
	SoaEdit          string    `json:"soa_edit,omitempty"`
	SoaEditApi       string    `json:"soa_edit_api,omitempty"`
	Comments         []Comment `json:"comments,omitempty"`
	Records          []Record  `json:"records,omitempty"`
}

type RRSets struct {
	RRSets []RRSet `json:"rrsets"`
}

type RRSet struct {
	Name       string    `json:"name"`
	Type       string    `json:"type"`
	ChangeType string    `json:"changetype"`
	Records    []Record  `json:"records,omitempty"`
	Comments   []Comment `json:"comments,omitempty"`
}

type Comment struct {
	Content    string `json:"content"`
	Account    string `json:"account"`
	ModifiedAt int    `json:"modified_at"`
	Type       string `json:"type,omitempty"`
	Name       string `json:"name,omitempty"`
}

type Record struct {
	Content  string `json:"content"`
	Name     string `json:"name"`
	TTL      int    `json:"ttl"`
	Type     string `json:"type"`
	Disabled bool   `json:"disabled"`
	SetPtr   bool   `json:"set-ptr,omitempty"`
	Priority int    `json:"priority"`
}

func marshalPowerDnsZonesToZones(pz []PowerDnsZone) []Zone {
	z := []Zone{}
	//GREG: Fix this
	return z
}

func marshalZonesToPowerDnsZones(z []Zone) []PowerDnsZone {
	pz := []PowerDnsZone{}
	//GREG: Fix this
	return pz
}

func marshalPowerDnsZoneToZone(pz PowerDnsZone) Zone {
	z := Zone{}
	//GREG: Fix this
	return z
}

func marshalZoneToPowerDnsZone(z Zone) PowerDnsZone {
	pz := PowerDnsZone{}
	//GREG: Fix this
	return pz
}

func marshalPowerDnsRRSetsToRRSets(prrs PowerDnsRRSets) RRSets {
	rrs := RRSets{}
	//GREG: Fix this
	return rrs
}

func marshalRRSetsToPowerDnsRRSets(rrs RRSets) PowerDnsRRSets {
	prrs := PowerDnsRRSets{}
	//GREG: Fix this
	return prrs
}
