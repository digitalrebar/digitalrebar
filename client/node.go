package client

import (
	"encoding/json"
	"fmt"
	"path"

	"github.com/digitalrebar/rebar-api/datatypes"
)

// Node wraps datatypes.Node to provide the client API.
type Node struct {
	datatypes.Node
	Timestamps
	apiHelper
}

// PowerActions gets the available power actions for this node.
func (o *Node) PowerActions() ([]string, error) {
	buf, err := session.request("GET", urlFor(o, "power"), nil)
	if err != nil {
		return nil, err
	}
	res := []string{}
	return res, json.Unmarshal(buf, &res)
}

// Move moves a Node from its current deployment to a new one.  It is
// guaranteed to be atomic.
func (o *Node) Move(depl *Deployment) error {
	o.DeploymentID = depl.ID
	return Update(o)
}

// Power performs a power management action for the node.
func (o *Node) Power(action string) error {
	_, err := session.request("PUT", fmt.Sprintf("%v?poweraction=%v", urlFor(o, "power"), action), nil)
	return err
}

// ActiveBootstate returns the current bootstate the node is in.  This
// can be different from the Bootenv attribute due to the provisioner
// not having gotten around to updating the boot environment.
func (o *Node) ActiveBootstate() string {
	attr := &Attrib{}
	attr.Name = "provisioner-active-bootstate"
	attr, err := GetAttrib(o, attr, "")
	if err != nil {
		return ""
	}
	if res, ok := attr.Value.(string); !ok {
		return ""
	} else {
		return res
	}
}

// Redeploy has a node redeploy itself from scratch.  This includes wiping out the
// filesystems, reconfiguring hardware, and reinstalling the OS and all roles.
func (o *Node) Redeploy() error {
	uri := urlFor(o, "redeploy")
	buf, err := session.request("PUT", uri, nil)
	if err != nil {
		return err
	}
	return unmarshal(uri, buf, o)
}

// Scrub tries to delete any noderoles on a node that are not in the
// deployment the node is currently a member of or any of that
// deployment's parents.
func (o *Node) Scrub() error {
	uri := urlFor(o, "scrub")
	buf, err := session.request("PUT", uri, nil)
	if err != nil {
		return err
	}
	return unmarshal(uri, buf, o)
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
func (o *Node) providers()          {}

// Deployment returns the Deployment the node is in.
func (o *Node) Deployment() (res *Deployment, err error) {
	res = &Deployment{}
	res.ID = o.DeploymentID
	err = Read(res)
	return res, err
}

// A Noder is anything that a node can be bound to.
type Noder interface {
	Crudder
	nodes()
}

// Nodes returns all the Nodes.
func Nodes(scope ...Noder) (res []*Node, err error) {
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = urlFor(scope[i])
	}
	paths = append(paths, "nodes")
	res = make([]*Node, 0)
	return res, List(path.Join(paths...), &res)
}
