package crowbar

import (
	"errors"
	"log"
	"strconv"
)

// AvailableHammer helps track what Hammers are available to be bound to a Node.
type AvailableHammer struct {
	ID        int64  `json:"id"`
	Priority  int64  `json:"priority"`
	Name      string `json:"name"`
	Type      string `json:"klass"`
	CreatedAt string `json:"created_at"`
	UpdatedAt string `json:"updated_at"`
	lastJson  []byte
}

func (o *AvailableHammer) Id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	} else if o.Name != "" {
		return o.Name
	} else {
		log.Panic("AvailableHammer has no ID or name")
		return ""
	}
}

func (o *AvailableHammer) SetId(s string) error {
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

func (o *AvailableHammer) ApiName() string {
	return "available_hammers"
}

func (o *AvailableHammer) setLastJSON(b []byte) {
	o.lastJson = make([]byte, len(b))
	copy(o.lastJson, b)
}

func (o *AvailableHammer) lastJSON() []byte {
	return o.lastJson
}

func (o *AvailableHammer) hammers() {}

func AvailableHammers() (res []*AvailableHammer, err error) {
	res = make([]*AvailableHammer, 0)
	return res, session.list(&res, "available_hammers")
}
