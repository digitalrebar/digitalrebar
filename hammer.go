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

type Hammer struct {
	ID                int64         `json:"id,omitempty"`
	Name              string        `json:"name,omitempty"`
	NodeID            int64         `json:"node_id,omitempty"`
	AvailableHammerID int64         `json:"available_hammer_id,omitempty"`
	Priority          int64         `json:"priority,omitempty"`
	Endpoint          string        `json:"endpoint,omitempty"`
	Username          string        `json:"username,omitempty"`
	Authenticator     string        `json:"authenticator,omitempty"`
	Actions           HammerActions `json:"actions,omitempty"`
	CreatedAt         string        `json:"created_at,omitempty"`
	UpdatedAt         string        `json:"updated_at,omitempty"`
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
