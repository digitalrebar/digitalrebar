package crowbar

import (
	"log"
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

func (o *NodeRole) Id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	}
	log.Panic("NodeRole has no ID")
	return ""
}

func (o *NodeRole) ApiName() string {
	return "node_roles"
}

func (o *NodeRole) Propose() error {
	return session.put(o, url(o,"propose"))
}

func (o *NodeRole) Commit() error {
	return session.put(o, url(o,"commit"))
}

func (o *NodeRole) Attribs() (res []*Attrib, err error) {
	return Attribs(url(o))
}

func NodeRoles(paths ...string) (res []*NodeRole, err error) {
	res = make([]*NodeRole,0)
	return res, session.list(&res,append(paths,"node_roles")...)
}
