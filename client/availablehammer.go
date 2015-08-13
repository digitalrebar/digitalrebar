package client

import "github.com/VictorLowther/crowbar-api/datatypes"

// AvailableHammer helps track what Hammers are available to be bound to a Node.
type AvailableHammer struct {
	datatypes.AvailableHammer
	Timestamps
	apiHelper
}

func (o *AvailableHammer) hammers() {}

func AvailableHammers() (res []*AvailableHammer, err error) {
	res = make([]*AvailableHammer, 0)
	return res, List("available_hammers", &res)
}
