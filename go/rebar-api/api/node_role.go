package api

import "github.com/digitalrebar/digitalrebar/go/rebar-api/datatypes"

type NodeRole struct {
	datatypes.NodeRole
	Timestamps
	apiHelper
	rebarSrc
}

func (o *NodeRole) attribs() {}

type NodeRoler interface {
	Crudder
	nodeRoles()
}

func (c *Client) NodeRoles(scope ...NodeRoler) (res []*NodeRole, err error) {
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = fragTo(scope[i])
	}
	nr := &NodeRole{}
	paths = append(paths, nr.ApiName())
	res = make([]*NodeRole, 0)
	return res, c.List(c.UrlFor(nr, paths...), &res)
}

// Force the noderole to retry
func (o *NodeRole) Retry() error {
	uri := o.client().UrlTo(o, "retry")
	buf, err := o.client().request("PUT", uri, nil)
	if err != nil {
		return err
	}
	return o.client().unmarshal(uri, buf, o)
}
