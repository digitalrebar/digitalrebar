package crowbar

import (
	"path"

	"github.com/VictorLowther/crowbar-api/datatypes"
)

type Role struct {
	datatypes.Role
	Timestamps
	apiHelper
}

func (o *Role) attribs()         {}
func (o *Role) nodes()           {}
func (o *Role) deployments()     {}
func (o *Role) deploymentRoles() {}
func (o *Role) nodeRoles()       {}

type Roler interface {
	Crudder
	roles()
}

func Roles(scope ...Roler) (res []*Role, err error) {
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = url(scope[i])
	}
	paths = append(paths, "roles")
	res = make([]*Role, 0)
	return res, List(path.Join(paths...), &res)
}
