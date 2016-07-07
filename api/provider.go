package api

import (
	"path"

	"github.com/digitalrebar/rebar-api/datatypes"
)

// Provider wraps datatypes.Provider to provide client API functionality
type Provider struct {
	datatypes.Provider
	Timestamps
	apiHelper
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
		paths[i] = urlFor(scope[i])
	}
	paths = append(paths, "providers")
	return res, c.List(path.Join(paths...), &res)
}
