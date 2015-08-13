package client

import "github.com/VictorLowther/crowbar-api/datatypes"

type Barclamp struct {
	datatypes.Barclamp
	Timestamps
	apiHelper
}

func Barclamps() (res []*Barclamp, err error) {
	res = make([]*Barclamp, 0)
	return res, List("barclamps", &res)
}
