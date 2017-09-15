package main

import (
	"encoding/json"
	"errors"
	"log"
	"net"
	"net/http"
	"sync"

	"github.com/digitalrebar/digitalrebar/go/common/store"
)

type DataTracker struct {
	sync.Mutex `json:"-"`
	store      store.SimpleStore
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
	dt.save_data(s.Name)
	return nil, http.StatusOK
}

func (dt *DataTracker) RemoveSubnet(subnetName string) (error, int) {
	lsubnet := dt.Subnets[subnetName]
	if lsubnet == nil {
		return errors.New("Not Found"), http.StatusNotFound
	}
	delete(dt.Subnets, subnetName)
	dt.remove_data(subnetName)
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

	delete(dt.Subnets, lsubnet.Name)

	// Make sure subnet doesn't overlap into other spaces.
	if dt.subnetsOverlap(subnet) {
		// Put the original back
		dt.Subnets[lsubnet.Name] = lsubnet
		return errors.New("Subnet overlaps with existing subnet"), http.StatusBadRequest
	}

	dt.Subnets[subnet.Name] = subnet
	dt.save_data(subnetName)
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
	keys, err := dt.store.Keys()
	log.Printf("Store keys: %v", keys)
	if err != nil {
		log.Panicf("Unable to fetch keys from the backing store")
	}
	if len(keys) == 1 && keys[0] == "subnets" {
		log.Printf("Migrating from v0 store format to v1 store format")
		buf, err := dt.store.Load("subnets")
		if err != nil {
			log.Panicf("Unable to load data from backing store: %s", err)
		}
		if err := json.Unmarshal(buf, &dt); err != nil {
			log.Panicf("Unable to unmarshal data from backing store: %s", err)
		}
		for k, v := range dt.Subnets {
			buf, err := json.Marshal(v)
			if err != nil {
				log.Panicf("Unable to marshal subnet %s: %v", k, err)
			}
			if err := dt.store.Save(k, buf); err != nil {
				log.Panicf("Unable to save subnet %s: %v", k, err)
			}
		}
		log.Printf("Migration complete.")
		dt.store.Remove("subnets")
		return
	}
	if dt.Subnets == nil {
		dt.Subnets = map[string]*Subnet{}
	}
	for _, key := range keys {
		if key == "subnets" {
			continue
		}
		buf, err := dt.store.Load(key)
		if err != nil {
			log.Panicf("Unable to load data for subnet %s: %v", key, err)
		}
		subnet := &Subnet{}
		if err := json.Unmarshal(buf, subnet); err != nil {
			log.Panicf("Unable to unmarshal subnet %s: %v", key, err)
		}
		dt.Subnets[key] = subnet
	}
}

func (dt *DataTracker) save_data(key string) {
	subn, ok := dt.Subnets[key]
	if !ok {
		return
	}
	buf, err := json.Marshal(subn)
	if err != nil {
		log.Panicf("Unable to marshal data to save to backing store: %s", err)
	}
	if err := dt.store.Save(key, buf); err != nil {
		log.Panicf("Unable to save data to backing store: %s", err)
	}
}

func (dt *DataTracker) remove_data(key string) {
	if _, ok := dt.Subnets[key]; !ok {
		return
	}
	if err := dt.store.Remove(key); err != nil {
		log.Panicf("Error removing subnet %s: %v", key, err)
	}
	delete(dt.Subnets, key)
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

	lsubnet.Bindings[binding.Mac] = &binding
	dt.save_data(subnetName)
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

	delete(lsubnet.Bindings, mac)
	dt.save_data(subnetName)
	return nil, http.StatusOK
}

func (dt *DataTracker) DeleteLease(subnetName, mac string) (error, int) {
	lsubnet := dt.Subnets[subnetName]
	if lsubnet == nil {
		return errors.New("Subnet Not Found"), http.StatusNotFound
	}

	b := lsubnet.Leases[mac]
	if b == nil {
		return errors.New("Lease Not Found"), http.StatusNotFound
	}

	delete(lsubnet.Leases, mac)
	dt.save_data(subnetName)
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
		dt.save_data(subnetName)
	}

	return nil, http.StatusOK
}
