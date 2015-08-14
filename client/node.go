package client

import (
	"encoding/json"
	"fmt"
	"path"

	"github.com/VictorLowther/crowbar-api/datatypes"
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
	old_deployment_id := o.DeploymentID
	tgt := fmt.Sprintf("%v?old_deployment_id=%v", urlFor(o), o.DeploymentID)
	o.DeploymentID = depl.ID
	inbuf, err := json.Marshal(o)
	_, err = session.request("PUT", tgt, inbuf)
	if err != nil {
		o.DeploymentID = old_deployment_id
	}
	return err
}

// Power performs a power management action for the node.
func (o *Node) Power(action string) error {
	_, err := session.request("PUT", fmt.Sprintf("power?poweraction=%v", action), nil)
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

// Satisfy salient interfaces
func (o *Node) attribs()            {}
func (o *Node) deploymentRoles()    {}
func (o *Node) nodeRoles()          {}
func (o *Node) hammers()            {}
func (o *Node) roles()              {}
func (o *Node) networks()           {}
func (o *Node) networkRanges()      {}
func (o *Node) networkAllocations() {}

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
