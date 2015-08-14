package client

import (
	"path"

	"github.com/VictorLowther/crowbar-api/datatypes"
)

type NodeRole struct {
	datatypes.NodeRole
	Timestamps
	apiHelper
}

func (o *NodeRole) attribs() {}

type NodeRoler interface {
	Crudder
	nodeRoles()
}

func NodeRoles(scope ...NodeRoler) (res []*NodeRole, err error) {
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = urlFor(scope[i])
	}
	paths = append(paths, "node_roles")
	res = make([]*NodeRole, 0)
	return res, List(path.Join(paths...), &res)
}
