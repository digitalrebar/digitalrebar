package api

import "github.com/digitalrebar/digitalrebar/go/rebar-api/datatypes"

// Provider wraps datatypes.Provider to provide client API functionality
type Provider struct {
	datatypes.Provider
	Timestamps
	apiHelper
	rebarSrc
}

// Providerer is anything that a Provider can be bound to.
type Providerer interface {
	Crudder
	providers()
}

// Providers returns all of the Providers.
func (c *Client) Providers(scope ...Providerer) (res []*Provider, err error) {
	res = make([]*Provider, 0)
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = fragTo(scope[i])
	}
	p := &Provider{}
	paths = append(paths, p.ApiName())
	return res, c.List(c.UrlFor(p, paths...), &res)
}
