package crowbar

import (
	"errors"
	"path"

	"github.com/VictorLowther/crowbar-api/datatypes"
)

type Deployment struct {
	datatypes.Deployment
	Timestamps
	apiHelper
}

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
	paths = append(paths, "deployments")
	return res, List(path.Join(paths...), &res)
}
