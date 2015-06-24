package crowbar

import (
	"log"
	"path"
	"strconv"
)

type Deployment struct {
	ID          int64  `json:"id,omitempty"`
	State       int    `json:"state,omitempty"`
	Name        string `json:"name,omitempty"`
	Description string `json:"description,omitempty"`
	System      bool   `json:"system,omitempty"`
	ParentID    int64  `json:"parent_id,omitempty"`
	CreatedAt   string `json:"created_at,omitempty"`
	UpdatedAt   string `json:"updated_at,omitempty"`
}

func (o *Deployment) id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	} else if o.Name != "" {
		return o.Name
	} else {
		log.Panic("Deployment has no ID or name")
		return ""
	}
}

func (o *Deployment) url(parts ...string) string {
	return path.Join(append([]string{"deployments",o.id()}, parts...)...)
}

func (o *Deployment) Get() error {
	return Get(o, o.url())
}

func (o *Deployment) Delete() error {
	return Delete(o.url())
}

func (o *Deployment) Create() error {
	return Post(o, "deployments")
}

func (o *Deployment) Update() error {
	return Put(o, o.url())
}

func (o *Deployment) Propose() error {
	return Put(o, o.url("propose"))
}

func (o *Deployment) Commit() error {
	return Put(o, o.url("commit"))
}

func (o *Deployment) Attribs() (res []*Attrib, err error) {
	res = []*Attrib{}
	err = Get(res, o.url("attribs"))
	return res, err
}

func (o *Deployment) GetAttrib(a *Attrib) error {
	return Get(a,o.url(a.url()))
}

func (o *Deployment) SetAttrib(a *Attrib) error {
	return Put(a,o.url(a.url()))
}

func Deployments() (res []*Deployment, err error) {
	res = []*Deployment{}
	err = Get(res, "deployments")
	return res, err
}
