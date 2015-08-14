package client

import (
	"errors"
	"path"

	"github.com/VictorLowther/crowbar-api/datatypes"
)

// Deployment wraps datatypes.Deployment to provide client API functionality
type Deployment struct {
	datatypes.Deployment
	Timestamps
	apiHelper
}

// Parent fetches the parent of the Deployment, if it has one.
func (o *Deployment) Parent() (res *Deployment, err error) {
	res = &Deployment{}
	if o.ParentID.Valid {
		res.ID = o.ParentID.Int64
		return res, Read(res)
	}
	return nil, errors.New("Deployment has no parent")
}

// Satisfy salient interfaces
func (o *Deployment) attribs()         {}
func (o *Deployment) deploymentRoles() {}
func (o *Deployment) nodes()           {}
func (o *Deployment) nodeRoles()       {}
func (o *Deployment) roles()           {}

// Deploymenters are things that are associated with or bound in the
// context of a Deployment.
type Deploymenter interface {
	Crudder
	deployments()
}

// Deployments returns all of the Deployments.
func Deployments(scope ...Deploymenter) (res []*Deployment, err error) {
	res = make([]*Deployment, 0)
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = urlFor(scope[i])
	}
	paths = append(paths, "deployments")
	return res, List(path.Join(paths...), &res)
}
