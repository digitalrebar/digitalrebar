package dhcp

import (
	"net"
	"time"
)

// Option id number from DHCP RFC 2132 and 2131
// Value is a string version of the value
type Option struct {
	Code  byte   `json:"id"`
	Value string `json:"value"`
}

type Lease struct {
	Ip         net.IP    `json:"ip"`
	Mac        string    `json:"mac"`
	Valid      bool      `json:"valid"`
	ExpireTime time.Time `json:"expire_time"`
}

type Binding struct {
	Ip         net.IP    `json:"ip"`
	Mac        string    `json:"mac"`
	Options    []*Option `json:"options,omitempty"`
	NextServer *string   `json:"next_server,omitempty"`
}

type Subnet struct {
	Name              string     `json:"name"`
	Subnet            string     `json:"subnet"`
	NextServer        string     `json:"next_server,omitempty"`
	ActiveStart       string     `json:"active_start"`
	ActiveEnd         string     `json:"active_end"`
	ActiveLeaseTime   int        `json:"active_lease_time"`
	ReservedLeaseTime int        `json:"reserved_lease_time"`
	OnlyBoundLeases   bool       `json:"only_bound_leases"`
	Leases            []*Lease   `json:"leases,omitempty"`
	Bindings          []*Binding `json:"bindings,omitempty"`
	Options           []*Option  `json:"options,omitempty"`
	TenantId          int        `json:"tenant_id"`
}

func (s *Subnet) ApiName() string {
	return "subnets"
}

func (s *Subnet) Id() (string, error) {
	return s.Name, nil
}

func (s *Subnet) SetId(name string) error {
	s.Name = name
	return nil
}
