// Example of minimal DHCP server:
package main

import (
	"encoding/binary"
	"encoding/json"
	dhcp "github.com/krolaw/dhcp4"
	"github.com/willf/bitset"
	"log"
	"net"
	"time"
)

type Subnet struct {
	Name              string
	Subnet            *MyIPNet
	NextServer        *net.IP `"json:,omitempty"`
	ActiveStart       net.IP
	ActiveEnd         net.IP
	ActiveLeaseTime   time.Duration
	ActiveBits        *bitset.BitSet
	ReservedLeaseTime time.Duration
	Leases            map[string]*Lease
	Bindings          map[string]*Binding
	Options           dhcp.Options // Options to send to DHCP Clients
}

func NewSubnet() *Subnet {
	return &Subnet{
		Leases:     make(map[string]*Lease),
		Bindings:   make(map[string]*Binding),
		Options:    make(dhcp.Options),
		ActiveBits: bitset.New(0),
	}
}

func (s *Subnet) MarshalJSON() ([]byte, error) {
	as := convertSubnetToApiSubnet(s)
	return json.Marshal(as)
}

func (s *Subnet) UnmarshalJSON(data []byte) error {
	var as ApiSubnet

	err := json.Unmarshal(data, &as)
	if err != nil {
		return err
	}

	if s.Leases == nil {
		s.Leases = make(map[string]*Lease)
	}
	if s.Bindings == nil {
		s.Bindings = make(map[string]*Binding)
	}
	if s.Options == nil {
		s.Options = make(dhcp.Options)
	}
	if s.ActiveBits == nil {
		s.ActiveBits = bitset.New(0)
	}
	_, err = convertApiSubnetToSubnet(&as, s)
	return err
}

func (subnet *Subnet) free_lease(dt *DataTracker, nic string) {
	lease := subnet.Leases[nic]
	if lease != nil {
		if dhcp.IPInRange(subnet.ActiveStart, subnet.ActiveEnd, lease.Ip) {
			subnet.ActiveBits.Clear(uint(dhcp.IPRange(lease.Ip, subnet.ActiveStart) - 1))
		}
		delete(subnet.Leases, nic)
		dt.save_data()
	}
}

func (subnet *Subnet) find_info(dt *DataTracker, nic string) (*Lease, *Binding) {
	return subnet.Leases[nic], subnet.Bindings[nic]
}

func firstClearBit(bs *bitset.BitSet) (uint, bool) {
	for i := uint(0); i < bs.Len(); i++ {
		if !bs.Test(i) {
			return i, true
		}
	}
	return 0, false
}

func (subnet *Subnet) getFreeIP() (*net.IP, bool) {
	bit, success := firstClearBit(subnet.ActiveBits)
	if success {
		subnet.ActiveBits.Set(bit)
		ip := dhcp.IPAdd(subnet.ActiveStart, int(bit))
		return &ip, true
	}

	// Free invalid or expired leases
	save_me := false
	now := time.Now()
	for k, lease := range subnet.Leases {
		if now.After(lease.ExpireTime) {
			if dhcp.IPInRange(subnet.ActiveStart, subnet.ActiveEnd, lease.Ip) {
				subnet.ActiveBits.Clear(uint(dhcp.IPRange(lease.Ip, subnet.ActiveStart) - 1))
			}
			delete(subnet.Leases, k)
			save_me = true
		}
	}

	bit, success = firstClearBit(subnet.ActiveBits)
	if success {
		subnet.ActiveBits.Set(bit)
		ip := dhcp.IPAdd(subnet.ActiveStart, int(bit))
		return &ip, true
	}

	// We got nothin'
	return nil, save_me
}

func (subnet *Subnet) find_or_get_info(dt *DataTracker, nic string, suggest net.IP) (*Lease, *Binding) {
	lease, binding := subnet.find_info(dt, nic)

	var theip *net.IP

	if binding != nil {
		theip = &binding.Ip
	}

	// Resolve potential conflicts.
	if lease != nil && binding != nil {
		if !lease.Ip.Equal(binding.Ip) {
			// Let the lease go.
			lease = nil
		}
	}

	if lease == nil {
		if theip == nil {
			var save_me bool
			theip, save_me = subnet.getFreeIP()
			if theip == nil {
				if save_me {
					dt.save_data()
				}
				return nil, nil
			}
		}
		lease = &Lease{
			Ip:    *theip,
			Mac:   nic,
			Valid: true,
		}
		subnet.Leases[nic] = lease
		dt.save_data()
	}

	return lease, binding
}

func (s *Subnet) update_lease_time(dt *DataTracker, lease *Lease, d time.Duration) {
	lease.ExpireTime = time.Now().Add(d)
	dt.save_data()
}

func (s *Subnet) build_options(lease *Lease, binding *Binding) (dhcp.Options, time.Duration) {
	var lt time.Duration
	if binding == nil {
		lt = s.ActiveLeaseTime
	} else {
		lt = s.ReservedLeaseTime
	}

	opts := make(dhcp.Options)

	// Build renewal / rebinding time options
	b := make([]byte, 4)
	binary.BigEndian.PutUint32(b, uint32(lt)/2)
	opts[dhcp.OptionRenewalTimeValue] = b
	b = make([]byte, 4)
	binary.BigEndian.PutUint32(b, uint32(lt)*3/4)
	opts[dhcp.OptionRebindingTimeValue] = b

	// fold in subnet options
	for c, v := range s.Options {
		opts[c] = v
	}

	// fold in binding options
	if binding != nil {
		for _, v := range binding.Options {
			b, err := convertOptionValueToByte(v.Code, v.Value)
			if err != nil {
				log.Println("Failed to parse option: ", v.Code, " ", v.Value)
			} else {
				opts[v.Code] = b
			}
		}
	}

	return opts, lt
}
