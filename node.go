package crowbar

import (
	"encoding/json"
	"errors"
	"fmt"
	"log"
	"strconv"
)

// Node represents a system (real or virtual) that Crowbar can manage.
type Node struct {
	// The numeric ID of the Node.  This is an opaque value that has
	// no meaning beyond being unique in the set of all Nodes.
	ID int64 `json:"id,omitempty"`
	// The name of the node.  This is generally the FQDN of the
	// node, and it must be unique in the set of all Nodes.
	Name string `json:"name,omitempty"`
	// The description of this Node.
	Description string `json:"description,omitempty"`
	// Whether this node can coordinate and act on behalf of Crowbar.
	// Any node that runs the Crowbar API should have this set.
	Admin bool `json:"admin,omitempty"`
	// A short alias that this node can be referred to by.  Deprecated.
	Alias string `json:"alias,omitempty"`
	// Whether the node is powered on and reachable by Crowbar.
	Alive bool `json:"alive,omitempty"`
	// I do not remember what this flag is used for.
	Allocated bool `json:"allocated,omitempty"`
	// Whether the node is available to the rest of the Crowbar infrastructure.
	// Nodes that are not alive and available will not have jobs run on them.
	Available bool `json:"available,omitempty"`
	// The current environment that the node should boot into.
	// This includes, but is not restricted to:
	//    * "local" for booting to local storage.
	//    * "<operating system>-install" for booting an OS install.
	//    * "sledgehammer" for booting to our discovery/inventory environment
	Bootenv string `json:"bootenv,omitempty"`
	// The deployment that this Node is currently a member of.
	DeploymentID int64 `json:"deployment_id,omitempty"`
	Order        int64 `json:"order,omitempty"`
	// Whether this node is available to run non-system roles on.
	System bool `json:"system,omitempty"`
	// The target Role that the annealer should converge the node to.
	// If it is 0, then the annealer will try to converge all the roles bound to the node.
	TargetRoleID int64  `json:"target_role_id,omitempty"`
	CreatedAt    string `json:"created_at,omitempty"`
	UpdatedAt    string `json:"updated_at,omitempty"`
}

func (o *Node) Id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	} else if o.Name != "" {
		return o.Name
	} else {
		log.Panic("Node has no ID or name")
		return ""
	}
}

func (o *Node) SetId(s string) error {
	if o.ID != 0 || o.Name != "" {
		return errors.New("SetId can only be used on an un-IDed object")
	}
	if id, err := strconv.ParseInt(s, 10, 64); err == nil {
		o.ID = id
	} else {
		o.Name = s
	}
	return nil
}

func (o *Node) ApiName() string {
	return "nodes"
}

func (o *Node) Match() (res []*Node, err error) {
	res = make([]*Node, 0)
	return res, session.match(o, &res, o.ApiName(), "match")
}

// PowerActions gets the available power actions for this node.
func (o *Node) PowerActions() ([]string, error) {
	buf, err := session.request("GET", url(o, "power"), nil)
	if err != nil {
		return nil, err
	}
	res := []string{}
	return res, json.Unmarshal(buf, &res)
}

func (o *Node) Move(depl *Deployment) error {
	old_deployment_id := o.DeploymentID
	tgt := fmt.Sprintf("%v?old_deployment_id=%v", url(o), o.DeploymentID)
	o.DeploymentID = depl.ID
	err := session.put(o, tgt)
	if err != nil {
		o.DeploymentID = old_deployment_id
	}
	return err
}

// Power performs a power managmeent action for the node.
func (o *Node) Power(action string) error {
	return session.put(o, url(o, fmt.Sprintf("power?poweraction=%v", action)))
}

func (o *Node) ActiveBootstate() string {
	attr := &Attrib{}
	attr, err := GetAttrib(o, &Attrib{Name: "provisioner-active-bootstate"}, "")
	if err != nil {
		return ""
	}
	if res, ok := attr.Value.(string); !ok {
		return ""
	} else {
		return res
	}
}

// Satisfy salient interfaces
func (o *Node) attribs()            {}
func (o *Node) deploymentRoles()    {}
func (o *Node) nodeRoles()          {}
func (o *Node) hammers()            {}
func (o *Node) roles()              {}
func (o *Node) networks()           {}
func (o *Node) networkRanges()      {}
func (o *Node) networkAllocations() {}

func (o *Node) Deployment() (res *Deployment, err error) {
	res = &Deployment{ID: o.DeploymentID}
	err = Read(res)
	return res, err
}

type Noder interface {
	Crudder
	nodes()
}

func Nodes(scope ...Noder) (res []*Node, err error) {
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = url(scope[i])
	}

	res = make([]*Node, 0)
	return res, session.list(&res, append(paths, "nodes")...)
}
