package client

// Deprecated: use api instead. client will not be updated

import (
	"path"

	"github.com/rackn/digitalrebar/go/rebar-api/datatypes"
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

// Force the noderole to retry
func (o *NodeRole) Retry() error {
	uri := urlFor(o, "retry")
	buf, err := session.request("PUT", uri, nil)
	if err != nil {
		return err
	}
	return unmarshal(uri, buf, o)
}
