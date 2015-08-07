package crowbar

import (
	"errors"
	"log"
	"strconv"
)

// Deployments are the main tool that Crowbar provides to group
// related nodes together.  They carry Deployment-specific
// configuration information (in the form of DeploymentRoles), Nodes
// (a node belongs to exactly one Deployment at any given point in
// time), and NodeRoles (NodeRoles remember which Deployment they were
// first committed in, and always use that when calculating the attrib
// data to be passed in to a run).
//
// Deployments are structured as a tree with the system deployment at
// the root.  All newly-discovered Nodes start in the system
// deployment, where they have their inital set of Roles bound to
// them.
//
// Deployment satisfies the Attriber interface.  GetAttrib and
// SetAttrib calls on a Deployment will be redirected to the
// appropriate DeploymentRole in this Deployment.
type Deployment struct {
	// The unique identifier for a Deployment.
	ID int64 `json:"id"`
	// The state a deployment is in
	State int `json:"state"`
	// The name of the Deployment.  Must be globally unique.
	Name string `json:"name"`
	// A breif description of what the Deployment is for.
	Description string `json:"description"`
	// Whether the deployment is a system deployment.  Right now,
	// there can be only one of these.
	System bool `json:"system"`
	// The ID of the deployment that is the parent of this one.
	ParentID  int64  `json:"parent_id"`
	CreatedAt string `json:"created_at"`
	UpdatedAt string `json:"updated_at"`
	lastJson  []byte
}

const (
	DeploymentError     = -1
	DeploymentProposed  = 0
	DeploymentCommitted = 1
	DeploymentActive    = 2
)

func (o *Deployment) Id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	} else if o.Name != "" {
		return o.Name
	} else {
		log.Panic("Deployment has no ID or name")
		return ""
	}
}

func (o *Deployment) ApiName() string {
	return "deployments"
}

// SetId sets the ID of an object.
//
// If s can be parsed as an int64 without error, the objects ID field will
// be populated with the results of that conversion, otherwise the Name
// field will be populated with passed string.
// An error will be returned if the object already has a set Name or ID field,
// or if the object does not have a Name field and the passed string cannot be
// parsed to an int64
func (o *Deployment) SetId(s string) error {
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

func (o *Deployment) Parent() (res *Deployment, err error) {
	res = &Deployment{ID: o.ParentID}
	return res, Read(res)
}

func (o *Deployment) Match() (res []*Deployment, err error) {
	res = make([]*Deployment, 0)
	return res, session.match(o, &res, o.ApiName(), "match")
}

func (o *Deployment) setLastJSON(b []byte) {
	o.lastJson = make([]byte, len(b))
	copy(o.lastJson, b)
}

func (o *Deployment) lastJSON() []byte {
	return o.lastJson
}

// Satisfy salient interfaces
func (o *Deployment) attribs()         {}
func (o *Deployment) deploymentRoles() {}
func (o *Deployment) nodes()           {}
func (o *Deployment) nodeRoles()       {}
func (o *Deployment) roles()           {}

type Deploymenter interface {
	Crudder
	deployments()
}

// Deployments returns all of the Deployments.
func Deployments(scope ...Deploymenter) (res []*Deployment, err error) {
	res = make([]*Deployment, 0)
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = url(scope[i])
	}

	return res, session.list(&res, append(paths, "deployments")...)
}
