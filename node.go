package crowbar

import (
	"log"
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

func (o *Node) Id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	} else if o.Name != "" {
		return o.Name
	} else {
		log.Panic("Node has no ID or name")
		return ""
	}
}

func (o *Node) ApiName() string {
	return "nodes"
}

func (o *Node) Propose() error {
	return session.put(o, url(o, "propose"))
}

func (o *Node) Commit() error {
	return session.put(o, url(o, "commit"))
}

func (o *Node) Attribs() (res []*Attrib, err error) {
	return Attribs(url(o))
}

func (o *Node) Deployment() (res *Deployment, err error) {
	res = &Deployment{ID: o.DeploymentID}
	err = Read(res)
	return res, err
}

func (o *Node) NodeRoles() (res []*NodeRole, err error) {
	return NodeRoles(url(o))
}

func Nodes(paths ...string) (res []*Node, err error) {
	res = make([]*Node, 0)
	return res, session.list(&res,append(paths, "nodes")...)
}
