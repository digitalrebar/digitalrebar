package main

import (
	"encoding/json"
	"errors"
	"log"
	"net"
	"net/http"
	"sync"

	"github.com/digitalrebar/digitalrebar/go/common/store"
	dhcp "github.com/krolaw/dhcp4"
)

type DataTracker struct {
	sync.Mutex `json:"-"`
	store      store.SimpleStore  `json:"-"`
	Subnets    map[string]*Subnet // subnet -> SubnetData
}

func NewDataTracker(store store.SimpleStore) *DataTracker {
	return &DataTracker{
		Subnets: make(map[string]*Subnet),
		store:   store,
	}
}

func (dt *DataTracker) FindBoundIP(mac string) *Subnet {
	for _, s := range dt.Subnets {
		for _, b := range s.Bindings {
			if b.Mac == mac {
				return s
			}
		}
	}
	return nil
}

func (dt *DataTracker) FindSubnet(ip net.IP) *Subnet {
	for _, s := range dt.Subnets {
		if s.Subnet.Contains(ip) {
			return s
		}
	}
	return nil
}

func (dt *DataTracker) AddSubnet(s *Subnet) (error, int) {
	lsubnet := dt.Subnets[s.Name]
	if lsubnet != nil {
		return errors.New("Already exists"), http.StatusConflict
	}

	// Make sure subnet doesn't overlap into other spaces.
	if dt.subnetsOverlap(s) {
		return errors.New("Subnet overlaps with existing subnet"), http.StatusBadRequest
	}

	dt.Subnets[s.Name] = s
	dt.save_data()
	return nil, http.StatusOK
}

func (dt *DataTracker) RemoveSubnet(subnetName string) (error, int) {
	lsubnet := dt.Subnets[subnetName]
	if lsubnet == nil {
		return errors.New("Not Found"), http.StatusNotFound
	}
	delete(dt.Subnets, subnetName)
	dt.save_data()
	return nil, http.StatusOK
}

func (dt *DataTracker) ReplaceSubnet(subnetName string, subnet *Subnet) (error, int) {
	lsubnet := dt.Subnets[subnetName]
	if lsubnet == nil {
		return errors.New("Not Found"), http.StatusNotFound
	}

	// Take Leases and Bindings from old to new if nets match
	subnet.Leases = lsubnet.Leases
	subnet.Bindings = lsubnet.Bindings

	// XXX: One day we should handle if active/reserved change.
	subnet.ActiveBits = lsubnet.ActiveBits

	delete(dt.Subnets, lsubnet.Name)

	// Make sure subnet doesn't overlap into other spaces.
	if dt.subnetsOverlap(subnet) {
		// Put the original back
		dt.Subnets[lsubnet.Name] = lsubnet
		return errors.New("Subnet overlaps with existing subnet"), http.StatusBadRequest
	}

	dt.Subnets[subnet.Name] = subnet
	dt.save_data()
	return nil, http.StatusOK
}

// HACK BECAUSE IPNet doesn't marshall/unmarshall
type MyIPNet struct {
	*net.IPNet
}

func (ipnet MyIPNet) MarshalText() ([]byte, error) {
	return []byte(ipnet.String()), nil
}

// UnmarshalText implements the encoding.TextUnmarshaler interfacee.
// The IP address is expected in a form accepted by ParseIP.
func (ipnet *MyIPNet) UnmarshalText(text []byte) error {
	if len(text) == 0 {
		return errors.New("Empty MyIPNet")
	}
	s := string(text)
	_, newnet, err := net.ParseCIDR(s)
	if err != nil {
		return &net.ParseError{"NetIP address", s}
	}
	*ipnet = MyIPNet{
		&net.IPNet{IP: newnet.IP, Mask: newnet.Mask},
	}
	return nil
}

/*
 * Data storage/retrieval functions
 */
func (dt *DataTracker) load_data() {
	buf, err := dt.store.Load("subnets")
	if _, ok := err.(store.NotFound); ok {
		return
	}
	if err != nil {
		log.Panicf("Unable to load data from backing store: %s", err)
	}
	if err := json.Unmarshal(buf, &dt); err != nil {
		log.Panicf("Unable to unmarshal data from backing store: %s", err)
	}
}

func (dt *DataTracker) save_data() {
	buf, err := json.Marshal(dt)
	if err != nil {
		log.Panicf("Unable to marshal data to save to backing store: %s", err)
	}
	if err := dt.store.Save("subnets", buf); err != nil {
		log.Panicf("Unable to save data to backing store: %s", err)
	}
}

func (dt *DataTracker) subnetsOverlap(subnet *Subnet) bool {
	for _, es := range dt.Subnets {
		if es.Subnet.Contains(subnet.Subnet.IP) {
			return true
		}
		if subnet.Subnet.Contains(es.Subnet.IP) {
			return true
		}
	}
	return false
}

func (dt *DataTracker) AddBinding(subnetName string, binding Binding) (error, int) {
	lsubnet := dt.Subnets[subnetName]
	if lsubnet == nil {
		return errors.New("Not Found"), http.StatusNotFound
	}

	// If existing, clear the reservation for IP
	b := lsubnet.Bindings[binding.Mac]
	if b != nil {
		if dhcp.IPInRange(lsubnet.ActiveStart, lsubnet.ActiveEnd, b.Ip) {
			lsubnet.ActiveBits.Clear(uint(dhcp.IPRange(lsubnet.ActiveStart, b.Ip) - 1))
		}
	}

	// Reserve the IP if in Active range
	if dhcp.IPInRange(lsubnet.ActiveStart, lsubnet.ActiveEnd, binding.Ip) {
		lsubnet.ActiveBits.Set(uint(dhcp.IPRange(lsubnet.ActiveStart, binding.Ip) - 1))
	}

	lsubnet.Bindings[binding.Mac] = &binding
	dt.save_data()
	return nil, http.StatusOK
}

func (dt *DataTracker) DeleteBinding(subnetName, mac string) (error, int) {
	lsubnet := dt.Subnets[subnetName]
	if lsubnet == nil {
		return errors.New("Subnet Not Found"), http.StatusNotFound
	}

	b := lsubnet.Bindings[mac]
	if b == nil {
		return errors.New("Binding Not Found"), http.StatusNotFound
	}

	if dhcp.IPInRange(lsubnet.ActiveStart, lsubnet.ActiveEnd, b.Ip) {
		lsubnet.ActiveBits.Clear(uint(dhcp.IPRange(lsubnet.ActiveStart, b.Ip) - 1))
	}

	delete(lsubnet.Bindings, mac)
	dt.save_data()
	return nil, http.StatusOK
}

func (dt *DataTracker) SetNextServer(subnetName string, ip net.IP, nextServer NextServer) (error, int) {
	lsubnet := dt.Subnets[subnetName]
	if lsubnet == nil {
		return errors.New("Not Found"), http.StatusNotFound
	}

	save_me := false
	for _, v := range lsubnet.Bindings {
		if v.Ip.Equal(ip) && (v.NextServer == nil || *v.NextServer != nextServer.Server) {
			save_me = true
			v.NextServer = &nextServer.Server
		}
	}

	if save_me {
		dt.save_data()
	}

	return nil, http.StatusOK
}
