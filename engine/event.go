package engine

/*
Copyright (c) 2016, Rackn Inc.
Licensed under the terms of the Digital Rebar License.
See LICENSE.md at the top of this repository for more information.
*/

import "github.com/digitalrebar/rebar-api/client"

// EventSelector is used by the Engine to register
// for the types of events we would like to recieve.
// It generally has the following fields:
//    event: The name of the event.  The only required field.
//    obj_class: The class of the Rebar object that fired the event.
//    obj_id: The human-readable name of the Rebar object that fired
//            the event, if any.
//
// There may be other fields added later -- the exact details of
// what is in an EventSelector are up to the Digital Rebar core code.
type EventSelector map[string]string

// Match against another selector to see if the second one
// has all the fields and values of the first.
func (es EventSelector) Match(other EventSelector) bool {
	for k, v := range es {
		val, ok := other[k]
		if !ok || val != v {
			return false
		}
	}
	return true
}

func (es EventSelector) forRebar() map[string]interface{} {
	res := map[string]interface{}{}
	for k, v := range es {
		res[k] = v
	}
	return res
}

// Event is what is recieved from the DigitalRebar core whenever
// something of interest occurs.  The current definition is subject to
// change as the needs of the Classifier grow.  Not all fields will be
// present for all events -- only Selector and Event are guaranteed
// to exist.  The other fields will be populated depending on the
// object that fired the Event:
//
// NodeRole: NodeRole, Node, Role, and Deployment will be filled.
//
// Node: Node and Deployment will be filled.
//
// DeploymentRole: Deployment, Role, and DeploymentRole will be filled.
//
// Deployment: only Deployment will be filled.
//
// Role: only Role will be filled.
//
// Network: only Network will be filled.
//
// NetworkAllocation: Network, Node, NetworkRange, and NetworkAllocation will be filled.
//
// NetworkRange: Network and NetworkRange will be filled.
//
// NetworkRouter: Network and NetworkRouter will be filled.
type Event struct {
	Selector          EventSelector             `json:"selector"`
	Event             *client.Event             `json:"event"`
	Node              *client.Node              `json:"node"`
	Role              *client.Role              `json:"role"`
	NodeRole          *client.NodeRole          `json:"node_role"`
	Deployment        *client.Deployment        `json:"deployment"`
	DeploymentRole    *client.DeploymentRole    `json:"deployment_role"`
	Network           *client.Network           `json:"network"`
	NetworkAllocation *client.NetworkAllocation `json:"network_allocation"`
	NetworkRange      *client.NetworkRange      `json:"network_range"`
	NetworkRouter     *client.NetworkRouter     `json:"network_router"`
}
