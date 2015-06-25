package crowbar

import (
	"log"
	"strconv"
)

type Attrib struct {
	ID          int64       `json:"id,omitempty"`
	Name        string      `json:"name,omitempty"`
	Description string      `json:"description,omitempty"`
	BarclampID  int64       `json:"barclamp_id,omitempty"`
	RoleID      int64       `json:"role_id,omitempty"`
	Type        string      `json:"type,omitempty"`
	Writable    bool        `json:"writable,omitempty"`
	Schema      interface{} `json:"schema,omitempty"`
	Map         string      `json:"map,omitempty"`
	Order       int64       `json:"order,omitempty"`
	Value       interface{} `json:"value,omitempty"`
	CreatedAt   string      `json:"created_at,omitempty"`
	UpdatedAt   string      `json:"updated_at,omitempty"`
}

func (o *Attrib) Id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	} else if o.Name != "" {
		return o.Name
	} else {
		log.Panic("Attrib has no ID or name")
		return ""
	}
}

func (o *Attrib) ApiName() string {
	return "attribs"
}

func Attribs(paths ...string) (res []*Attrib, err error) {
	res = make([]*Attrib, 0)
	return res, session.list(&res,append(paths, "attribs")...)
}
