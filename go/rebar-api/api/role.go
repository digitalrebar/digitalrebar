package api

import "github.com/digitalrebar/digitalrebar/go/rebar-api/datatypes"

// Role wraps datatypes.Role to provide the client API.
type Role struct {
	datatypes.Role
	Timestamps
	apiHelper
	rebarSrc
}

func (o *Role) attribs()         {}
func (o *Role) nodes()           {}
func (o *Role) deployments()     {}
func (o *Role) deploymentRoles() {}
func (o *Role) nodeRoles()       {}

// A Roler is anything that a Role can be bound to.
type Roler interface {
	Crudder
	roles()
}

// Roles returns all the Roles.
func (c *Client) Roles(scope ...Roler) (res []*Role, err error) {
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = fragTo(scope[i])
	}
	r := &Role{}
	paths = append(paths, r.ApiName())
	res = make([]*Role, 0)
	return res, c.List(c.UrlFor(r, paths...), &res)
}
