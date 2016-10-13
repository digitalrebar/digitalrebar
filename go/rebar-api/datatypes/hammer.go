package datatypes

import (
	"path"

	"github.com/guregu/null"
)

type HammerActions struct {
	Power    []string `json:"power"`
	Transfer []string `json:"xfer"`
	Run      []string `json:"run"`
}

// Hammer is a binding between an AvailableHammer and a Node.
type Hammer struct {
	SimpleID
	Name string `json:"name"`
	// The ID of the Node that this Hammer works with.
	NodeID int64 `json:"node_id"`
	// The ID of the AvailableHammer that this Hammer works through.
	AvailableHammerID int64 `json:"available_hammer_id"`
	// The Priority of this Hammer as it alloes to this Node.  If
	// two Hammers provide the same Action, then the Hammer with
	// the higher Prioirty will be used.
	Priority int64 `json:"priority"`
	// The Endpoint that this Hammer talks to in order to perform
	// its action.  The exact format of this Endpoint is
	// AvailableHammer specific.
	Endpoint null.String `json:"endpoint"`
	// Username is the user that this Hammer shoud authenticate
	// against the Endpoint with.  Not all Hammers require a
	// Username
	Username string `json:"username"`
	// Authenticator is an opaque token that should be used to
	// authenticate with.
	Authenticator null.String `json:"authenticator"`
	// Actions define what non-idempotent Actions this Hammer can take.
	Actions HammerActions `json:"actions"`
}

func (o *Hammer) ApiName() string {
	return "hammers"
}

func (o *Hammer) ApiPath() string {
	return path.Join(API_PATH, o.ApiName())
}
