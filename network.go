package crowbar

import (
	"log"
	"fmt"
	"strconv"
	"errors"
)

type Network struct {
	ID           int64  `json:"id"`
	DeploymentID int64  `json:"deployment_id"`
	Vlan         int64  `json:"vlan"`
	TeamingMode  int64  `json:"team_mode"`
	UseTeam      bool   `json:"use_team"`
	UseVlan      bool   `json:"use_vlan"`
	UseBridge    bool   `json:"use_bridge"`
	Configure    bool   `json:"configure"`
	Name         string `json:"name"`
	Description  string `json:"description"`
	V6Prefix     string `json:"v6prefix"`
	Conduit      string `json:"conduit"`
	Category     string `json:"category"`
	Group        string `json:"group"`
}

func (o *Network) Id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	} else if o.Name != "" {
		return o.Name
	} else {
		log.Panic("Network has no ID or name")
		return ""
	}
}

func (o *Network) SetId(s string) error {
	if o.ID != 0 || o.Name != "" {
		return errors.New("SetId can only be used on an un-IDed object")
	}
	if id, err := strconv.ParseInt(s,10,64); err == nil {
		o.ID = id
	} else {
		o.Name = s
	}
	return nil
}

func (o *Network) ApiName() string {
	return "networks"
}

func (o *Network) Role() (role *Role, err error) {
	role = &Role{Name: fmt.Sprintf("network-%v", o.Name)}
	return role, Read(role)
}

// Satisfy salient interfaces
func (o *Network) networkRanges(){}
func (o *Network) networkAllocations(){}

type Networker interface{
	Crudder
	networks()
}

// Networks returns all of the Networks.
func Networks(scope ...Networker) (res []*Network, err error) {
	paths := make([]string,len(scope))
	for i := range scope {
		paths[i] = url(scope[i])
	}

	res = make([]*Network, 0)
	return res, session.list(&res, append(paths, "networks")...)
}

type NetworkRange struct {
	ID          int64  `json:"id"`
	NetworkID   int64  `json:"network_id"`
	Vlan        int64  `json:"vlan"`
	TeamingMode int64  `json:"team_mode"`
	UseTeam     bool   `json:"use_team"`
	UseVlan     bool   `json:"use_vlan"`
	UseBridge   bool   `json:"use_bridge"`
	Overlap     bool   `json:"overlap"`
	Conduit     string `json:"conduit"`
	First       string `json:"first"`
	Last        string `json:"last"`
	Name        string `json:"string"`
}

func (o *NetworkRange) Id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	} else if o.Name != "" {
		return o.Name
	} else {
		log.Panic("NetworkRange has no ID or name")
		return ""
	}
}

func (o *NetworkRange) SetId(s string) error {
	if o.ID != 0 {
		return errors.New("SetId can only be used on an un-IDed object")
	}
	if id, err := strconv.ParseInt(s,10,64); err == nil {
		o.ID = id
	} else {
		return err
	}
	return nil
}

func (o *NetworkRange) ApiName() string {
	return "network_ranges"
}

func (o *NetworkRange) Network() (*Network, error) {
	res := &Network{ID: o.NetworkID}
	return res, Read(res)
}

func (o *NetworkRange) networkAllocations(){}

type NetworkRanger interface {
	Crudder
	networkRanges()
}

// Networks returns all of the Networks.
func NetworkRanges(scope ...NetworkRanger) (res []*NetworkRange, err error) {
	paths := make([]string,len(scope))
	for i := range scope {
		paths[i] = url(scope[i])
	}

	res = make([]*NetworkRange, 0)
	return res, session.list(&res, append(paths, "network_ranges")...)
}

type NetworkAllocation struct {
	ID             int64  `json:"id"`
	NodeID         int64  `json:"node_id"`
	NetworkID      int64  `json:"network_id"`
	NetworkRangeID int64  `json:"network_range_id"`
	Address        string `json:"address"`
}

func (o *NetworkAllocation) Id() string {
	return strconv.FormatInt(o.ID, 10)
}

func (o *NetworkAllocation) SetId(s string) error {
	if o.ID != 0 {
		return errors.New("SetId can only be used on an un-IDed object")
	}
	if id, err := strconv.ParseInt(s,10,64); err == nil {
		o.ID = id
	} else {
		return err
	}
	return nil
}

func (o *NetworkAllocation) ApiName() string {
	return "network_allocations"
}

func (o *NetworkAllocation) Node() (*Node, error) {
	res := &Node{ID: o.NodeID}
	return res, Read(res)
}

func (o *NetworkAllocation) Network() (*Network, error) {
	res := &Network{ID: o.NetworkID}
	return res, Read(res)
}

type NetworkAllocater interface {
	Crudder
	networkAllocations()
}

// Networks returns all of the Networks.
func NetworkAllocations(scope ...NetworkAllocater) (res []*NetworkAllocation, err error) {
	paths := make([]string,len(scope))
	for i := range scope {
		paths[i] = url(scope[i])
	}

	res = make([]*NetworkAllocation, 0)
	return res, session.list(&res, append(paths, "network_allocations")...)
}
