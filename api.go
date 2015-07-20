package main

import (
	"github.com/ant0ine/go-json-rest/rest"
	dhcp "github.com/krolaw/dhcp4"
	"net"
	"net/http"
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

	subnet, err := convertApiSubnetToSubnet(apisubnet, nil)
	if err != nil {
		rest.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	err, code := fe.DhcpInfo.AddSubnet(subnet)
	if err != nil {
		rest.Error(w, err.Error(), code)
		return
	}

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

	subnet, err := convertApiSubnetToSubnet(apisubnet, nil)
	if err != nil {
		rest.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	err, code := fe.DhcpInfo.ReplaceSubnet(subnetName, subnet)
	if err != nil {
		rest.Error(w, err.Error(), code)
		return
	}

	w.WriteJson(apisubnet)
}

// Delete function
func (fe *Frontend) DeleteSubnet(w rest.ResponseWriter, r *rest.Request) {
	subnetName := r.PathParam("id")

	err, code := fe.DhcpInfo.RemoveSubnet(subnetName)
	if err != nil {
		rest.Error(w, err.Error(), code)
		return
	}

	w.WriteHeader(code)
}

func (fe *Frontend) BindSubnet(w rest.ResponseWriter, r *rest.Request) {
	subnetName := r.PathParam("id")
	binding := Binding{}
	err := r.DecodeJsonPayload(&binding)
	if err != nil {
		rest.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	err, code := fe.DhcpInfo.AddBinding(subnetName, binding)
	if err != nil {
		rest.Error(w, err.Error(), code)
		return
	}

	w.WriteJson(binding)
}

func (fe *Frontend) UnbindSubnet(w rest.ResponseWriter, r *rest.Request) {
	subnetName := r.PathParam("id")
	mac := r.PathParam("mac")

	err, code := fe.DhcpInfo.DeleteBinding(subnetName, mac)
	if err != nil {
		rest.Error(w, err.Error(), code)
		return
	}

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

	err, code := fe.DhcpInfo.SetNextServer(subnetName, ip, nextServer)
	if err != nil {
		rest.Error(w, err.Error(), code)
		return
	}

	w.WriteJson(nextServer)
}
