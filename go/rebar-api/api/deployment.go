package api

import (
	"errors"
	"path"

	"github.com/digitalrebar/digitalrebar/go/rebar-api/datatypes"
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
		return res, o.client().Read(res)
	}
	return nil, errors.New("Deployment has no parent")
}

// Redeploy will redeploy everything in the deployment.  This includes
// wiping out the filesystems, reconfiguring hardware, and
// reinstalling the OS and all roles on all the nodes in the
// deployment.
func (o *Deployment) Redeploy() error {
	uri := urlFor(o, "redeploy")
	buf, err := o.client().request("PUT", uri, nil)
	if err != nil {
		return err
	}
	return o.client().unmarshal(uri, buf, o)
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
func (c *Client) Deployments(scope ...Deploymenter) (res []*Deployment, err error) {
	res = make([]*Deployment, 0)
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = fragTo(scope[i])
	}
	paths = append(paths, "deployments")
	return res, c.List(path.Join(datatypes.API_PATH, path.Join(paths...)), &res)
}
