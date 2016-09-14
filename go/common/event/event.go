package event

import (
	"reflect"

	"github.com/digitalrebar/rebar-api/api"
)

// EventSelector is used to register with the Rebar API for the types
// of events something would like to recieve.
//
// It generally has the following fields:
//    event: The name of the event.  The only required field.
//    obj_class: The class of the Rebar object that fired the event.
//    obj_id: The human-readable name of the Rebar object that fired
//            the event, if any.
//
// There may be other fields added later -- the exact details of
// what is in an EventSelector are up to the Digital Rebar core code.
type Selector map[string]interface{}

// Match against another selector to see if the second one
// has all the fields and values of the first.
func (es Selector) Match(other Selector) bool {
	for k, v := range es {
		val, ok := other[k]
		if !ok || !reflect.DeepEqual(val, v) {
			return false
		}
	}
	return true
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
	Selector          Selector               `json:"selector"`
	Event             *api.Event             `json:"event"`
	Node              *api.Node              `json:"node"`
	Role              *api.Role              `json:"role"`
	NodeRole          *api.NodeRole          `json:"node_role"`
	Deployment        *api.Deployment        `json:"deployment"`
	DeploymentRole    *api.DeploymentRole    `json:"deployment_role"`
	Network           *api.Network           `json:"network"`
	NetworkAllocation *api.NetworkAllocation `json:"network_allocation"`
	NetworkRange      *api.NetworkRange      `json:"network_range"`
	NetworkRouter     *api.NetworkRouter     `json:"network_router"`
}

// Source makes a guess at the Rebar object class that caused the Event to be issued.
func (e *Event) Source() string {
	if e.NetworkRange != nil {
		return "NetworkRange"
	}
	if e.NetworkRouter != nil {
		return "NetworkRouter"
	}
	if e.NetworkAllocation != nil {
		return "NetworkAllocation"
	}
	if e.Network != nil {
		return "Network"
	}
	if e.DeploymentRole != nil {
		return "DeploymentRole"
	}
	if e.NodeRole != nil {
		return "NodeRole"
	}
	if e.Node != nil {
		return "Node"
	}
	if e.Deployment != nil {
		return "Deployment"
	}
	if e.Role != nil {
		return "Role"
	}
	return "Unknown"
}
