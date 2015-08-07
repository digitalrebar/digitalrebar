package crowbar

import (
	"path"

	"github.com/VictorLowther/crowbar-api/datatypes"
)

type Jig struct {
	datatypes.Jig
	Timestamps
	apiHelper
}

type Jigger interface {
	Crudder
	jigs()
}

// Jigs returns all of the Jigs.
func Jigs(scope ...Jigger) (res []*Jig, err error) {
	res = make([]*Jig, 0)
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = url(scope[i])
	}
	paths = append(paths, "jigs")
	return res, List(path.Join(paths...), &res)
}
