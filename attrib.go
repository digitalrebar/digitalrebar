package crowbar

import (
	"log"
	"path"
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

func (o *Attrib) id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	} else if o.Name != "" {
		return o.Name
	} else {
		log.Panic("Attrib has no ID or name")
		return ""
	}
}

func (o *Attrib) url(parts ...string) string {
	return path.Join(append([]string{"attribs",o.id()}, parts...)...)
}

func (o *Attrib) Get() error {
	return Get(o, o.url())
}

func (o *Attrib) Delete() error {
	return Delete(o.url())
}

func (o *Attrib) Create() error {
	return Post(o, "attribs")
}

func (o *Attrib) Update() error {
	return Put(o, o.url())
}

func (o *Attrib) Propose() error {
	return Put(o, o.url("propose"))
}

func (o *Attrib) Commit() error {
	return Put(o, o.url("commit"))
}

func Attribs() (res []*Attrib, err error) {
	res = []*Attrib{}
	err = Get(res, "attribs")
	return res, err
}
