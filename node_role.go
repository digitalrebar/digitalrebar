package crowbar

import (
	"log"
	"strconv"
)

// NodeRole represents the binding of a Role to a Node in a Deployment.
// All of them together are linked in a graph that defines how everything
// in the cluster is related.
type NodeRole struct {
	// The ID of the NodeRole.  It is an opaque integer that is
	// unique among all the NodeRoles
	ID              int64       `json:"id,omitempty"`
	// The Deployment that this noderole was created or committed
	// in.  While they are in Proposed, a noderole will follow the
	// node when it changes deployments.  Once the NodeRole is
	// committed for the first time, however, it will be bound to
	// the Deployment it was committed in, and will use that
	// deplpyment's DeploymentRoles to construct Attribs for jobs.
	DeploymentID    int64       `json:"deployment_id,omitempty"`
	// The Role that this noderole is bound to.
	RoleID          int64       `json:"role_id,omitempty"`
	// The Node that this noderole is bound to.  The
	// (NodeID,RoleID) pair must be unique -- it is not possible
	// to bind the same Role to a Node multiple times.
	NodeID          int64       `json:"node_id,omitempty"`
	// The state a NodeRole is in.  
	State           int         `json:"state,omitempty"`
	// The status of the NodeRole.  This string is settable by the user.
	Status          string      `json:"status,omitempty"`
	// The log from the last time this NodeRole ran against its Node.
	RunLog          string      `json:"runlog,omitempty"`
	// Whether this NodeRole is available to the annealer
	Available       bool        `json:"available,omitempty"`
	Order           int         `json:"order,omitempty"`
	NodeError       bool        `json:"node_error,omitempty"`
	CreatedAt       string      `json:"created_at,omitempty"`
	UpdatedAt       string      `json:"updated_at,omitempty"`
}

const (
	NodeRoleError = -1
	NodeRoleActive = 0
	NodeRoleTodo = 1
	NodeRoleTransition = 2
	NodeRoleBlocked = 3
	NodeRoleProposed = 4
)

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

func (o *NodeRole) Attribs() (res []*Attrib, err error) {
	return Attribs(url(o))
}

func NodeRoles(paths ...string) (res []*NodeRole, err error) {
	res = make([]*NodeRole,0)
	return res, session.list(&res,append(paths,"node_roles")...)
}
