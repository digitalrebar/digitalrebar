package crowbar

import (
	"log"
	"strconv"
)
// DeploymentRoles represent a binding of a Role to a Deployment.
// They are where the deployment-specific configuration information
// for Roles are stored (as Attribs on the DeploymentRole).
// DeploymentRoles havce the following attrib buckets:
//
//    * proposed
//    * committed
//    * system
//
// DeploymentRole satisfies the Attriber interface.
type DeploymentRole struct {
	ID              int64       `json:"id,omitempty"`
	DeploymentID    int64       `json:"deployment_id,omitempty"`
	RoleID          int64       `json:"role_id,omitempty"`
	CreatedAt       string      `json:"created_at,omitempty"`
	UpdatedAt       string      `json:"updated_at,omitempty"`
}

func (o *DeploymentRole) Id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	}
	log.Panic("DeploymentRole has no ID")
	return ""
}

func (o *DeploymentRole) ApiName() string {
	return "deployment_roles"
}

func (o *DeploymentRole) Attribs() (res []*Attrib, err error) {
	return Attribs(url(o))
}

// Deployment returns the Deployment that this DeploymentRole is bound to.
func (o *DeploymentRole) Deployment() (res *Deployment, err error) {
	res = &Deployment{ID:o.DeploymentID}
	return res, Read(res)
}

// Role returns the Role that this DeploymentRole is bound to.
func (o *DeploymentRole) Role() (res *Role, err error) {
	res = &Role{ID:o.RoleID}
	return res, Read(res)
}

// DeploymentRoles returns all the DeplymentRoles at the passed Path.
func DeploymentRoles(paths ...string) (res []*DeploymentRole, err error) {
	res = make([]*DeploymentRole,0)
	return res, session.list(&res,append(paths,"deployment_roles")...)
}
