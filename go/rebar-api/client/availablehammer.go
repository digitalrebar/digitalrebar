package client

// Deprecated: use api instead. client will not be updated

import "github.com/digitalrebar/digitalrebar/go/rebar-api/datatypes"

// AvailableHammer helps track what Hammers are available to be bound to a Node.
type AvailableHammer struct {
	datatypes.AvailableHammer
	Timestamps
	apiHelper
}

func (o *AvailableHammer) hammers() {}

// AvailableHammers returns all of the available Hammers that can be
// bound to a node.
func AvailableHammers() (res []*AvailableHammer, err error) {
	res = make([]*AvailableHammer, 0)
	return res, List("available_hammers", &res)
}
