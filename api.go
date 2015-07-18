package main

import (
	"encoding/json"
	"github.com/ant0ine/go-json-rest/rest"
	dhcp "github.com/krolaw/dhcp4"
	"io/ioutil"
	"log"
	"net"
	"net/http"
	"sync"
	"time"
)

/*
 * Managment API Structures
 *
 * These are the management API structures
 *
 * These match the json objects that are needed to
 * update/create and get subnets information and records
 *
 * Includes bind and unbind actions.
 */
type ApiSubnet struct {
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
}

// Option id number from DHCP RFC 2132 and 2131
// Value is a string version of the value
type Option struct {
	Code  dhcp.OptionCode `json:"id"`
	Value string          `json:"value"`
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

type NextServer struct {
	Server string `json:"next_server"`
}

func NewApiSubnet() *ApiSubnet {
	return &ApiSubnet{
		Leases:   make([]*Lease, 0),
		Bindings: make([]*Binding, 0),
		Options:  make([]*Option, 0),
	}
}

func NewBinding() *Binding {
	return &Binding{
		Options: make([]*Option, 0),
	}
}

type DataTracker struct {
	Subnets  map[string]*Subnet // subnet -> SubnetData
	data_dir string             `json:"-"`
	Lock     sync.Mutex         `json:"-"`
}

func NewDataTracker(data_dir string) *DataTracker {
	return &DataTracker{
		Subnets:  make(map[string]*Subnet),
		data_dir: data_dir,
	}
}

func (dt *DataTracker) FindSubnet(ip net.IP) *Subnet {
	for _, s := range dt.Subnets {
		if s.Subnet.Contains(ip) {
			return s
		}
	}
	return nil
}

/*
 * Structure for the front end with a pointer to the backend
 */
type Frontend struct {
	DhcpInfo *DataTracker
}

func NewFrontend(data_dir string) *Frontend {
	fe := &Frontend{
		DhcpInfo: NewDataTracker(data_dir),
	}

	fe.DhcpInfo.load_data()

	return fe
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
		ipnet = nil
		return nil
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
	bytes, err := ioutil.ReadFile(dt.data_dir + "/database.json")
	if err != nil {
		log.Panic("failed to read file", err.Error())
	}

	err = json.Unmarshal(bytes, dt)
	if err != nil {
		log.Panic("failed to parse file", err.Error())
	}
}

func (dt *DataTracker) save_data() {
	jdata, err := json.Marshal(dt)
	if err != nil {
		log.Panic("Failed to marshal data", err.Error())
	}
	err = ioutil.WriteFile(dt.data_dir+"/database.json", jdata, 0700)
	if err != nil {
		log.Panic("Failed to save data", err.Error())
	}
}

func (dt *DataTracker) subnetsOverlap(subnet *Subnet) bool {
	for _, es := range dt.Subnets {
		if es.Name == subnet.Name {
			continue
		}
		if es.Subnet.Contains(subnet.Subnet.IP) {
			return true
		}
		if subnet.Subnet.Contains(es.Subnet.IP) {
			return true
		}
	}
	return false
}

// List function
func (fe *Frontend) GetAllSubnets(w rest.ResponseWriter, r *rest.Request) {
	nets := make([]*ApiSubnet, 0)

	for _, s := range fe.DhcpInfo.Subnets {
		as := convertSubnetToApiSubnet(s)
		nets = append(nets, as)
	}

	w.WriteJson(nets)
}

// Get function
func (fe *Frontend) GetSubnet(w rest.ResponseWriter, r *rest.Request) {
	subnetName := r.PathParam("id")

	subnet := fe.DhcpInfo.Subnets[subnetName]
	if subnet == nil {
		rest.Error(w, "Not Found", http.StatusNotFound)
		return
	}
	w.WriteJson(convertSubnetToApiSubnet(subnet))
}

// Create function
func (fe *Frontend) CreateSubnet(w rest.ResponseWriter, r *rest.Request) {
	apisubnet := NewApiSubnet()
	err := r.DecodeJsonPayload(&apisubnet)
	if err != nil {
		rest.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	lsubnet := fe.DhcpInfo.Subnets[apisubnet.Name]
	if lsubnet != nil {
		rest.Error(w, "Already exists", http.StatusConflict)
		return
	}

	subnet, err := convertApiSubnetToSubnet(apisubnet, nil)
	if err != nil {
		rest.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	// Make sure subnet doesn't overlap into other spaces.
	if fe.DhcpInfo.subnetsOverlap(subnet) {
		rest.Error(w, "Subnet overlaps with existing subnet", http.StatusBadRequest)
		return
	}

	fe.DhcpInfo.Subnets[apisubnet.Name] = subnet
	fe.DhcpInfo.save_data()

	w.WriteJson(apisubnet)
}

// Update function
func (fe *Frontend) UpdateSubnet(w rest.ResponseWriter, r *rest.Request) {
	subnetName := r.PathParam("id")
	apisubnet := NewApiSubnet()
	err := r.DecodeJsonPayload(&apisubnet)
	if err != nil {
		rest.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	lsubnet := fe.DhcpInfo.Subnets[subnetName]
	if lsubnet == nil {
		rest.Error(w, "Not Found", http.StatusNotFound)
		return
	}

	subnet, err := convertApiSubnetToSubnet(apisubnet, nil)
	if err != nil {
		rest.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	// Make sure subnet doesn't overlap into other spaces.
	if fe.DhcpInfo.subnetsOverlap(subnet) {
		rest.Error(w, "Subnet overlaps with existing subnet", http.StatusBadRequest)
		return
	}

	// XXX: One day we should handle if active/reserved change.

	// Take Leases and Bindings from old to new if nets match
	subnet.Leases = lsubnet.Leases
	subnet.Bindings = lsubnet.Bindings
	subnet.ActiveBits = lsubnet.ActiveBits

	if lsubnet.Name != subnet.Name {
		delete(fe.DhcpInfo.Subnets, lsubnet.Name)
	}
	fe.DhcpInfo.Subnets[subnet.Name] = subnet
	fe.DhcpInfo.save_data()

	w.WriteJson(apisubnet)
}

// Delete function
func (fe *Frontend) DeleteSubnet(w rest.ResponseWriter, r *rest.Request) {
	subnetName := r.PathParam("id")

	lsubnet := fe.DhcpInfo.Subnets[subnetName]
	if lsubnet == nil {
		rest.Error(w, "Not Found", http.StatusNotFound)
		return
	}

	delete(fe.DhcpInfo.Subnets, subnetName)
	fe.DhcpInfo.save_data()

	w.WriteHeader(http.StatusOK)
}

func (fe *Frontend) BindSubnet(w rest.ResponseWriter, r *rest.Request) {
	subnetName := r.PathParam("id")
	binding := Binding{}
	err := r.DecodeJsonPayload(&binding)
	if err != nil {
		rest.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	lsubnet := fe.DhcpInfo.Subnets[subnetName]
	if lsubnet == nil {
		rest.Error(w, "Not Found", http.StatusNotFound)
		return
	}

	if dhcp.IPInRange(lsubnet.ActiveStart, lsubnet.ActiveEnd, binding.Ip) {
		lsubnet.ActiveBits.Set(uint(dhcp.IPRange(lsubnet.ActiveStart, binding.Ip) - 1))
	}

	lsubnet.Bindings[binding.Mac] = &binding
	fe.DhcpInfo.save_data()

	w.WriteJson(binding)
}

func (fe *Frontend) UnbindSubnet(w rest.ResponseWriter, r *rest.Request) {
	subnetName := r.PathParam("id")
	mac := r.PathParam("mac")

	lsubnet := fe.DhcpInfo.Subnets[subnetName]
	if lsubnet == nil {
		rest.Error(w, "Not Found", http.StatusNotFound)
		return
	}

	b := lsubnet.Bindings[mac]
	if b == nil {
		rest.Error(w, "Not Found", http.StatusNotFound)
		return
	}

	delete(lsubnet.Bindings, mac)
	fe.DhcpInfo.save_data()

	w.WriteHeader(http.StatusOK)
}

func (fe *Frontend) NextServer(w rest.ResponseWriter, r *rest.Request) {
	subnetName := r.PathParam("id")
	nextServer := NextServer{}
	err := r.DecodeJsonPayload(&nextServer)
	if err != nil {
		rest.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	ip := net.ParseIP(r.PathParam("ip"))

	lsubnet := fe.DhcpInfo.Subnets[subnetName]
	if lsubnet == nil {
		rest.Error(w, "Not Found", http.StatusNotFound)
		return
	}

	save_me := false
	for _, v := range lsubnet.Bindings {
		if v.Ip.Equal(ip) && (v.NextServer == nil || *v.NextServer != nextServer.Server) {
			save_me = true
			v.NextServer = &nextServer.Server
		}
	}

	if save_me {
		fe.DhcpInfo.save_data()
	}

	w.WriteJson(nextServer)
}
