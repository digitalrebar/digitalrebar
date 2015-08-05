package crowbar

import (
	"errors"
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
	ID           int64  `json:"id,omitempty"`
	DeploymentID int64  `json:"deployment_id,omitempty"`
	RoleID       int64  `json:"role_id,omitempty"`
	CreatedAt    string `json:"created_at,omitempty"`
	UpdatedAt    string `json:"updated_at,omitempty"`
	lastJson     []byte
}

func (o *DeploymentRole) Id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	}
	log.Panic("DeploymentRole has no ID")
	return ""
}

func (o *DeploymentRole) SetId(s string) error {
	if o.ID != 0 {
		return errors.New("SetId can only be used on an un-IDed object")
	}
	if id, err := strconv.ParseInt(s, 10, 64); err == nil {
		o.ID = id
	} else {
		return err
	}
	return nil
}

func (o *DeploymentRole) ApiName() string {
	return "deployment_roles"
}

func (o *DeploymentRole) Match() (res []*DeploymentRole, err error) {
	res = make([]*DeploymentRole, 0)
	return res, session.match(o, &res, o.ApiName(), "match")
}

func (o *DeploymentRole) setLastJSON(b []byte) {
	o.lastJson = make([]byte, len(b))
	copy(o.lastJson, b)
}

func (o *DeploymentRole) lastJSON() []byte {
	return o.lastJson
}

// Satisfy salient interfaces.
func (o *DeploymentRole) attribs()   {}
func (o *DeploymentRole) nodes()     {}
func (o *DeploymentRole) nodeRoles() {}

// Deployment returns the Deployment that this DeploymentRole is bound to.
func (o *DeploymentRole) Deployment() (res *Deployment, err error) {
	res = &Deployment{ID: o.DeploymentID}
	return res, Read(res)
}

// Role returns the Role that this DeploymentRole is bound to.
func (o *DeploymentRole) Role() (res *Role, err error) {
	res = &Role{ID: o.RoleID}
	return res, Read(res)
}

// DeploymentRoler is an interface that anything that has related DeploymentRoles can satisfy to
// get its related DeploymentRoles
type DeploymentRoler interface {
	Crudder
	deploymentRoles()
}

// DeploymentRoles returns all the DeplymentRoles at the passed Path.
func DeploymentRoles(scope ...DeploymentRoler) (res []*DeploymentRole, err error) {
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = url(scope[i])
	}
	res = make([]*DeploymentRole, 0)
	return res, session.list(&res, append(paths, "deployment_roles")...)
}
