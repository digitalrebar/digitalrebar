package crowbar

import (
	"log"
	"path"
	"strconv"
)

type NodeRole struct {
	ID              int64       `json:"id,omitempty"`
	DeploymentID    int64       `json:"deployment_id,omitempty"`
	RoleID          int64       `json:"role_id,omitempty"`
	NodeID          int64       `json:"node_id,omitempty"`
	State           int         `json:"state,omitempty"`
	Status          string      `json:"status,omitempty"`
	RunLog          string      `json:"runlog,omitempty"`
	Available       bool        `json:"available,omitempty"`
	Order           int         `json:"order,omitempty"`
	NodeError       bool        `json:"node_error,omitempty"`
	CreatedAt       string      `json:"created_at,omitempty"`
	UpdatedAt       string      `json:"updated_at,omitempty"`
}

func (o *NodeRole) id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	}
	log.Panic("NodeRole has no ID")
	return ""
}

func (o *NodeRole) url(parts ...string) string {
	return path.Join(append([]string{"noderoles", o.id()}, parts...)...)
}

func (o *NodeRole) Get() error {
	return Get(o, o.url())
}

func (o *NodeRole) Delete() error {
	return Delete(o.url())
}

func (o *NodeRole) Create() error {
	return Post(o, "noderoles")
}

func (o *NodeRole) Update() error {
	return Put(o, o.url())
}

func (o *NodeRole) Propose() error {
	return Put(o, o.url("propose"))
}

func (o *NodeRole) Commit() error {
	return Put(o, o.url("commit"))
}

func (o *NodeRole) Attribs() (res []*Attrib, err error) {
	res = []*Attrib{}
	err = Get(res, o.url("attribs"))
	return res, err
}

func (o *NodeRole) GetAttrib(a *Attrib) error {
	return Get(a, o.url(a.url()))
}

func (o *NodeRole) SetAttrib(a *Attrib) error {
	return Put(a, o.url(a.url()))
}

func NodeRoles() (res []*NodeRole, err error) {
	res = []*NodeRole{}
	err = Get(res, "noderoles")
	return res, err
}
