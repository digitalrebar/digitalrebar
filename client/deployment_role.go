package client

import (
	"path"

	"github.com/VictorLowther/crowbar-api/datatypes"
)

// DeploymentRole wraps datatypes.DeploymentRole to add client API functionality.
type DeploymentRole struct {
	datatypes.DeploymentRole
	Timestamps
	apiHelper
}

// Satisfy salient interfaces.
func (o *DeploymentRole) attribs()   {}
func (o *DeploymentRole) nodes()     {}
func (o *DeploymentRole) nodeRoles() {}

// Deployment returns the Deployment that this DeploymentRole is bound to.
func (o *DeploymentRole) Deployment() (res *Deployment, err error) {
	res = &Deployment{}
	res.ID = o.DeploymentID
	return res, Read(res)
}

// Role returns the Role that this DeploymentRole is bound to.
func (o *DeploymentRole) Role() (res *Role, err error) {
	res = &Role{}
	res.ID = o.RoleID
	return res, Read(res)
}

// DeploymentRoler is an interface that anything that has related
// DeploymentRoles can satisfy to get its related DeploymentRoles
type DeploymentRoler interface {
	Crudder
	deploymentRoles()
}

// DeploymentRoles returns all the DeplymentRoles at the passed Path.
func DeploymentRoles(scope ...DeploymentRoler) (res []*DeploymentRole, err error) {
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = urlFor(scope[i])
	}
	paths = append(paths, "deployment_roles")
	res = make([]*DeploymentRole, 0)
	return res, List(path.Join(paths...), &res)
}
