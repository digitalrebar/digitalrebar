package crowbar

import (
	"log"
	"path"
	"strconv"
)

type Node struct {
	ID           int64  `json:"id,omitempty"`
	Name         string `json:"name,omitempty"`
	Description  string `json:"description,omitempty"`
	Admin        bool   `json:"admin,omitempty"`
	Alias        string `json:"alias,omitempty"`
	Alive        bool   `json:"alive,omitempty"`
	Allocated    bool   `json:"allocated,omitempty"`
	Available    bool   `json:"available,omitempty"`
	Bootenv      string `json:"bootenv,omitempty"`
	DeploymentID int64  `json:"deployment_id,omitempty"`
	Order        int64  `json:"order,omitempty"`
	System       bool   `json:"system,omitempty"`
	TargetRoleID int64  `json:"target_role_id,omitempty"`
	CreatedAt    string `json:"created_at,omitempty"`
	UpdatedAt    string `json:"updated_at,omitempty"`
}

func (o *Node) id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	} else if o.Name != "" {
		return o.Name
	} else {
		log.Panic("Node has no ID or name")
		return ""
	}
}

func (o *Node) url(parts ...string) string {
	return path.Join(append([]string{"nodes", o.id()}, parts...)...)
}

func (o *Node) Get() error {
	return Get(o, o.url())
}

func (o *Node) Delete() error {
	return Delete(o.url())
}

func (o *Node) Create() error {
	return Post(o, "nodes")
}

func (o *Node) Update() error {
	return Put(o, o.url())
}

func (o *Node) Propose() error {
	return Put(o, o.url("propose"))
}

func (o *Node) Commit() error {
	return Put(o, o.url("commit"))
}

func (o *Node) Attribs() (res []*Attrib, err error) {
	res = []*Attrib{}
	err = Get(res, o.url("attribs"))
	return res, err
}

func (o *Node) GetAttrib(a *Attrib) error {
	return Get(a, o.url(a.url()))
}

func (o *Node) SetAttrib(a *Attrib) error {
	return Put(a, o.url(a.url()))
}

func Nodes() (res []*Node, err error) {
	res = []*Node{}
	err = Get(res, "nodes")
	return res, err
}
