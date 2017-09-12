package api

import "github.com/digitalrebar/digitalrebar/go/rebar-api/datatypes"

type Group struct {
	datatypes.Group
	Timestamps
	apiHelper
	rebarSrc
}

func (o *Group) nodes() {}

type Grouper interface {
	Crudder
	groups()
}

func (c *Client) Groups(scope ...Grouper) (res []*Group, err error) {
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = fragTo(scope[i])
	}
	g := &Group{}
	paths = append(paths, g.ApiName())
	res = []*Group{}
	return res, c.List(c.UrlFor(g, paths...), &res)
}

func (c *Client) AddGroup(o Grouper, g *Group) ([]*Group, error) {
	uri := c.UrlTo(o, fragTo(g))
	_, err := c.request("PUT", uri, nil)
	if err != nil {
		return nil, err
	}
	return c.Groups(o)
}

func (c *Client) RemoveGroup(o Grouper, g *Group) ([]*Group, error) {
	uri := c.UrlTo(o, fragTo(g))
	_, err := c.request("DELETE", uri, nil)
	if err != nil {
		return nil, err
	}
	return c.Groups(o)
}
