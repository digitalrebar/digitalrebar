// Example of minimal DHCP server:

package main

import (
	"bytes"
	"encoding/binary"
	"encoding/json"
	"errors"
	"log"
	"net"
	"text/template"
	"time"

	dhcp "github.com/krolaw/dhcp4"
	"github.com/willf/bitset"
)

// Option id number from DHCP RFC 2132 and 2131
// Value is a string version of the value
type Option struct {
	Code  dhcp.OptionCode `json:"id"`
	Value string          `json:"value"`
}

func (o *Option) RenderToDHCP(srcOpts map[int]string) (code dhcp.OptionCode, val []byte, err error) {
	code = dhcp.OptionCode(o.Code)
	tmpl, err := template.New("dhcp_option").Parse(o.Value)
	if err != nil {
		return code, nil, err
	}
	buf := &bytes.Buffer{}
	if err := tmpl.Execute(buf, srcOpts); err != nil {
		return code, nil, err
	}
	val, err = convertOptionValueToByte(code, buf.String())
	return code, val, err
}

type Lease struct {
	Ip         net.IP    `json:"ip"`
	Mac        string    `json:"mac"`
	Valid      bool      `json:"valid"`
	ExpireTime time.Time `json:"expire_time"`
}

func (l *Lease) Phantom() bool {
	addr, _ := net.ParseMAC(l.Mac)
	return addr[0] == 00 && addr[1] == 0x53
}

type Binding struct {
	Ip         net.IP    `json:"ip"`
	Mac        string    `json:"mac"`
	Options    []*Option `json:"options,omitempty"`
	NextServer *string   `json:"next_server,omitempty"`
}

type Subnet struct {
	Name              string
	Subnet            *MyIPNet
	NextServer        *net.IP `json:",omitempty"`
	ActiveStart       net.IP
	ActiveEnd         net.IP
	ActiveLeaseTime   time.Duration
	ReservedLeaseTime time.Duration
	Leases            map[string]*Lease
	Bindings          map[string]*Binding
	Options           []*Option // Options to send to DHCP Clients
	TenantId          int
}

func NewSubnet() *Subnet {
	return &Subnet{
		Leases:   make(map[string]*Lease),
		Bindings: make(map[string]*Binding),
		Options:  make([]*Option, 0),
	}
}

type apiSubnet struct {
	Name              string     `json:"name"`
	Subnet            string     `json:"subnet"`
	NextServer        *string    `json:"next_server,omitempty"`
	ActiveStart       string     `json:"active_start"`
	ActiveEnd         string     `json:"active_end"`
	ActiveLeaseTime   int        `json:"active_lease_time"`
	ReservedLeaseTime int        `json:"reserved_lease_time"`
	Leases            []*Lease   `json:"leases,omitempty"`
	Bindings          []*Binding `json:"bindings,omitempty"`
	Options           []*Option  `json:"options,omitempty"`
	TenantId          int        `json:"tenant_id"`
}

func (s *Subnet) MarshalJSON() ([]byte, error) {
	as := &apiSubnet{
		Name:              s.Name,
		Subnet:            s.Subnet.String(),
		ActiveStart:       s.ActiveStart.String(),
		ActiveEnd:         s.ActiveEnd.String(),
		ActiveLeaseTime:   int(s.ActiveLeaseTime.Seconds()),
		ReservedLeaseTime: int(s.ReservedLeaseTime.Seconds()),
		Options:           s.Options,
		Leases:            make([]*Lease, len(s.Leases)),
		Bindings:          make([]*Binding, len(s.Bindings)),
		TenantId:          s.TenantId,
	}
	if s.NextServer != nil {
		ns := s.NextServer.String()
		as.NextServer = &ns
	}
	i := int64(0)
	for _, lease := range s.Leases {
		as.Leases[i] = lease
		i++
	}
	i = int64(0)
	for _, binding := range s.Bindings {
		as.Bindings[i] = binding
		i++
	}
	return json.Marshal(as)
}

func (s *Subnet) UnmarshalJSON(data []byte) error {
	as := &apiSubnet{}
	if err := json.Unmarshal(data, &as); err != nil {
		return err
	}
	s.Name = as.Name
	_, netdata, err := net.ParseCIDR(as.Subnet)
	if err != nil {
		return err
	} else {
		s.Subnet = &MyIPNet{netdata}
	}
	s.ActiveStart = net.ParseIP(as.ActiveStart).To4()
	s.ActiveEnd = net.ParseIP(as.ActiveEnd).To4()

	if !netdata.Contains(s.ActiveStart) {
		return errors.New("ActiveStart not in Subnet")
	}
	if !netdata.Contains(s.ActiveEnd) {
		return errors.New("ActiveEnd not in Subnet")
	}

	s.ActiveLeaseTime = time.Duration(as.ActiveLeaseTime) * time.Second
	s.ReservedLeaseTime = time.Duration(as.ReservedLeaseTime) * time.Second
	if as.NextServer != nil {
		ip := net.ParseIP(*as.NextServer).To4()
		s.NextServer = &ip
	}
	if s.ActiveLeaseTime == 0 {
		s.ActiveLeaseTime = 300 * time.Second
	}
	if s.ReservedLeaseTime == 0 {
		s.ReservedLeaseTime = 2 * time.Hour
	}
	if s.Leases == nil {
		s.Leases = map[string]*Lease{}
	}

	for _, v := range as.Leases {
		s.Leases[v.Mac] = v
	}

	if s.Bindings == nil {
		s.Bindings = map[string]*Binding{}
	}

	for _, v := range as.Bindings {
		s.Bindings[v.Mac] = v
	}

	s.Options = as.Options
	s.TenantId = as.TenantId
	mask := net.IP([]byte(net.IP(netdata.Mask).To4()))
	bcastBits := binary.BigEndian.Uint32(netdata.IP) | ^binary.BigEndian.Uint32(mask)
	buf := make([]byte, 4)
	binary.BigEndian.PutUint32(buf, bcastBits)
	s.Options = append(s.Options, &Option{dhcp.OptionSubnetMask, mask.String()})
	s.Options = append(s.Options, &Option{dhcp.OptionBroadcastAddress, net.IP(buf).String()})
	return nil
}

func (subnet *Subnet) freeLease(dt *DataTracker, nic string) {
	lease := subnet.Leases[nic]
	if lease != nil {
		delete(subnet.Leases, nic)
		dt.save_data()
	}
}

func (s *Subnet) InRange(addr net.IP) bool {
	return bytes.Compare(addr, s.ActiveStart) >= 0 &&
		bytes.Compare(addr, s.ActiveEnd) <= 0
}

func (subnet *Subnet) findInfo(dt *DataTracker, nic string) (*Lease, *Binding) {
	l := subnet.Leases[nic]
	b := subnet.Bindings[nic]
	return l, b
}

func firstClearBit(bs *bitset.BitSet) (uint, bool) {
	for i := uint(0); i < bs.Len(); i++ {
		if !bs.Test(i) {
			return i, true
		}
	}
	return 0, false
}

// This will need to be updated to be more efficient with larger subnets.
// Class C and below should be fine, however.
func (subnet *Subnet) getFreeIP() (*net.IP, bool) {
	// Free invalid or expired leases
	used := bitset.New(0)
	saveMe := false
	for k, v := range subnet.Leases {
		// If the lease has expired, whack it.
		if time.Now().After(v.ExpireTime) {
			delete(subnet.Leases, k)
			saveMe = true
			continue
		}
		// If the lease is out of range and does not have a static binding,
		// mark it as invalid.  This will cause it to be NAK'ed the next time
		// the client checks in.
		if _, found := subnet.Bindings[k]; !found && !subnet.InRange(v.Ip) {
			v.Valid = false
			saveMe = true
			continue
		}
		// If an invaild lease was made valid again due to a range change, make it valid again.
		if !v.Valid && !v.Phantom() {
			v.Valid = true
			saveMe = true
		}
		used.Set(uint(dhcp.IPRange(subnet.ActiveStart, v.Ip) - 1))
	}
	// Check bindings as well to keep it real.
	for _, v := range subnet.Bindings {
		if subnet.InRange(v.Ip) {
			used.Set(uint(dhcp.IPRange(subnet.ActiveStart, v.Ip) - 1))
		}
	}
	bit, success := firstClearBit(used)
	if success {
		ip := dhcp.IPAdd(subnet.ActiveStart, int(bit))
		//log.Printf("Returning bit = %v ip = %v\n", bit, ip)
		return &ip, true
	}
	return nil, saveMe
}

func (subnet *Subnet) findOrGetInfo(dt *DataTracker, nic string, suggest net.IP) (*Lease, *Binding) {
	// Fast path to see if we have a good lease
	binding := subnet.Bindings[nic]
	lease := subnet.Leases[nic]

	var theip *net.IP

	if binding != nil {
		theip = &binding.Ip
	}

	// Resolve potential conflicts.
	if lease != nil && binding != nil {
		if lease.Ip.Equal(binding.Ip) {
			return lease, binding
		}
		lease = nil
	}

	if lease == nil {
		// Slow path to see if we have can get a lease
		// Make sure nothing sneaked in
		lease = subnet.Leases[nic]
		binding = subnet.Bindings[nic]
		theip = nil
		if binding != nil {
			theip = &binding.Ip
		}
		// Resolve potential conflicts.
		if lease != nil && binding != nil {
			if lease.Ip.Equal(binding.Ip) {
				return lease, binding
			}
		}

		if theip == nil {
			var saveMe bool
			theip, saveMe = subnet.getFreeIP()
			if theip == nil {
				if saveMe {
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

func (s *Subnet) updateLeaseTime(dt *DataTracker, lease *Lease, d time.Duration) {
	lease.ExpireTime = time.Now().Add(d)
	dt.save_data()
}

func (s *Subnet) phantomLease(dt *DataTracker, nic string) {
	lease, _ := s.findInfo(dt, nic)
	if lease == nil {
		return
	}
	addr, err := net.ParseMAC(nic)
	if err != nil {
		return
	}
	addr[0] = 0x00
	addr[1] = 0x53
	lease.Valid = false
	lease.ExpireTime = time.Now().Add(30 * time.Second)
	lease.Mac = addr.String()
	delete(s.Leases, nic)
	s.Leases[lease.Mac] = lease
	dt.save_data()
}

func (s *Subnet) buildOptions(lease *Lease, binding *Binding, p dhcp.Packet) (dhcp.Options, time.Duration) {
	var lt time.Duration
	if binding == nil {
		lt = s.ActiveLeaseTime
	} else {
		lt = s.ReservedLeaseTime
	}

	opts := make(dhcp.Options)
	srcOpts := map[int]string{}
	for c, v := range p.ParseOptions() {
		srcOpts[int(c)] = convertByteToOptionValue(c, v)
		log.Printf("Recieved option: %v: %v", c, srcOpts[int(c)])
	}

	// Build renewal / rebinding time options
	b := make([]byte, 4)
	binary.BigEndian.PutUint32(b, uint32(lt/time.Second)/2)
	opts[dhcp.OptionRenewalTimeValue] = b
	b = make([]byte, 4)
	binary.BigEndian.PutUint32(b, uint32(lt/time.Second)*3/4)
	opts[dhcp.OptionRebindingTimeValue] = b

	// fold in subnet options
	for _, opt := range s.Options {
		c, v, err := opt.RenderToDHCP(srcOpts)
		if err != nil {
			log.Printf("Failed to render option %v: %v, %v\n", opt.Code, opt.Value, err)
			continue
		}
		opts[c] = v
	}

	// fold in binding options
	if binding != nil {
		for _, opt := range binding.Options {
			c, v, err := opt.RenderToDHCP(srcOpts)
			if err != nil {
				log.Printf("Failed to render option %v: %v, %v\n", opt.Code, opt.Value, err)
				continue
			}
			opts[c] = v
		}
	}

	return opts, lt
}
