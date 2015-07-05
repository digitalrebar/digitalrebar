package crowbar

import (
	"errors"
	"log"
	"strconv"
)

type AvailableHammer struct {
	ID        int64  `json:"id,omitempty"`
	Priority  int64  `json:"priority"`
	Name      string `json:"name"`
	Type      string `json:"klass,omitempty"`
	CreatedAt string `json:"created_at,omitempty"`
	UpdatedAt string `json:"updated_at,omitempty"`
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

func (o *AvailableHammer) hammers() {}

func AvailableHammers() (res []*AvailableHammer, err error) {
	res = make([]*AvailableHammer, 0)
	return res, session.list(&res, "available_hammers")
}
