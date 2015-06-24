package crowbar

import (
	"encoding/json"
	"log"
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

func (o *Deployment) Id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	} else if o.Name != "" {
		return o.Name
	} else {
		log.Panic("Deployment has no ID or name")
		return ""
	}
}

func (o *Deployment) ApiName() string {
	return "deployments"
}

func (o *Deployment) Propose() error {
	return session.put(o, url(o, "propose"))
}

func (o *Deployment) Commit() error {
	return session.put(o, url(o, "commit"))
}

func (o *Deployment) Attribs() (res []*Attrib, err error) {
	return Attribs(url(o))
}

func (o *Deployment) GetAttrib(a *Attrib) error {
	return session.get(a, url(o, url(a)))
}

func (o *Deployment) SetAttrib(a *Attrib) error {
	return session.put(a, url(o, url(a)))
}

func (o *Deployment) Nodes() (res []*Node, err error) {
	return Nodes(url(o))
}

func Deployments(paths ...string) (res []*Deployment, err error) {
	res = make([]*Deployment, 0)
	buf, err := session.list(append(paths, "deployments")...)
	if err != nil {
		return nil, err
	}
	err = json.Unmarshal(buf, &res)
	return res, err
}
