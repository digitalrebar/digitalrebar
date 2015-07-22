package crowbar

import (
	"errors"
	"log"
	"strconv"
)

type HammerActions struct {
	Power    []string `json:"power,omitempty"`
	Transfer []string `json:"xfer,omitempty"`
	Run      []string `json:"run,omitempty"`
}

// Hammer is a binding between an AvailableHammer and a Node.
type Hammer struct {
	// The ID of the Hammer.
	ID int64 `json:"id,omitempty"`
	// The Name of the Hammer.  It is the same as the name of the
	// AvailableHammer to which it is bound
	Name string `json:"name,omitempty"`
	// The ID of the Node that this Hammer works with.
	NodeID int64 `json:"node_id,omitempty"`
	// The ID of the AvailableHammer that this Hammer works through.
	AvailableHammerID int64 `json:"available_hammer_id,omitempty"`
	// The Priority of this Hammer as it alloes to this Node.  If
	// two Hammers provide the same Action, then the Hammer with
	// the higher Prioirty will be used.
	Priority int64 `json:"priority,omitempty"`
	// The Endpoint that this Hammer talks to in order to perform
	// its action.  The exact format of this Endpoint is
	// AvailableHammer specific.
	Endpoint string `json:"endpoint,omitempty"`
	// Username is the user that this Hammer shoud authenticate
	// against the Endpoint with.  Not all Hammers require a
	// Username
	Username string `json:"username,omitempty"`
	// Authenticator is an opaque token that should be used to
	// authenticate with.
	Authenticator string `json:"authenticator,omitempty"`
	// Actions define what non-idempotent Actions this Hammer can take.
	Actions   HammerActions `json:"actions,omitempty"`
	CreatedAt string        `json:"created_at,omitempty"`
	UpdatedAt string        `json:"updated_at,omitempty"`
}

func (o *Hammer) Id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	} else if o.Name != "" {
		return o.Name
	} else {
		log.Panic("Hammer has no ID or name")
		return ""
	}
}

func (o *Hammer) SetId(s string) error {
	if o.ID != 0 || o.Name != "" {
		return errors.New("SetId can only be used on an un-IDed object")
	}
	if id, err := strconv.ParseInt(s, 10, 64); err == nil {
		o.ID = id
	} else {
		o.Name = s
	}
	return nil
}

func (o *Hammer) ApiName() string {
	return "hammers"
}

func (o *Hammer) Match() (res []*Hammer, err error) {
	res = make([]*Hammer, 0)
	return res, session.match(o, &res, o.ApiName(), "match")
}

type Hammerer interface {
	Crudder
	hammers()
}

// Hammers returns all of the Hammers.
func Hammers(scope ...Hammerer) (res []*Hammer, err error) {
	res = make([]*Hammer, 0)
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = url(scope[i])
	}

	return res, session.list(&res, append(paths, "hammers")...)
}
