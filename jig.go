package crowbar

import (
	"errors"
	"log"
	"strconv"
)

type Jig struct {
	ID             int64  `json:"id,omitempty"`
	Name           string `json:"name,omitempty"`
	Description    string `json:"description,omitempty"`
	Active         bool   `json:"active"`
	ClientRoleName string `json:"client_role_name,omitempty"`
	Server         string `json:"server,omitempty"`
	ClientName     string `json:"client_name,omitempty"`
	Key            string `json:"key,omitempty"`
	CreatedAt      string `json:"created_at,omitempty"`
	UpdatedAt      string `json:"updated_at,omitempty"`
}

func (o *Jig) Id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	} else if o.Name != "" {
		return o.Name
	} else {
		log.Panic("Jig has no ID or name")
		return ""
	}
}

func (o *Jig) SetId(s string) error {
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

func (o *Jig) ApiName() string {
	return "jigs"
}

type Jigger interface {
	Crudder
	jigs()
}

// Jigs returns all of the Jigs.
func Jigs(scope ...Jigger) (res []*Jig, err error) {
	res = make([]*Jig, 0)
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = url(scope[i])
	}

	return res, session.list(&res, append(paths, "jigs")...)
}
