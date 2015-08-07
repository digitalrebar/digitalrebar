package crowbar

import (
	"errors"
	"log"
	"strconv"
)

// NodeRole represents the binding of a Role to a Node in a Deployment.
// All of them together are linked in a graph that defines how everything
// in the cluster is related.
type NodeRole struct {
	// The ID of the NodeRole.  It is an opaque integer that is
	// unique among all the NodeRoles
	ID int64 `json:"id"`
	// The Deployment that this noderole was created or committed
	// in.  While they are in Proposed, a noderole will follow the
	// node when it changes deployments.  Once the NodeRole is
	// committed for the first time, however, it will be bound to
	// the Deployment it was committed in, and will use that
	// deplpyment's DeploymentRoles to construct Attribs for jobs.
	DeploymentID int64 `json:"deployment_id"`
	// The Role that this noderole is bound to.
	RoleID int64 `json:"role_id"`
	// The Node that this noderole is bound to.  The
	// (NodeID,RoleID) pair must be unique -- it is not possible
	// to bind the same Role to a Node multiple times.
	NodeID int64 `json:"node_id"`
	// The state a NodeRole is in.
	State int `json:"state"`
	// The status of the NodeRole.  This string is settable by the user.
	Status string `json:"status"`
	// The log from the last time this NodeRole ran against its Node.
	RunLog string `json:"runlog"`
	// Whether this NodeRole is available to the annealer
	Available bool   `json:"available"`
	Order     int    `json:"order"`
	NodeError bool   `json:"node_error"`
	CreatedAt string `json:"created_at"`
	UpdatedAt string `json:"updated_at"`
	lastJson  []byte
}

const (
	NodeRoleError      = -1
	NodeRoleActive     = 0
	NodeRoleTodo       = 1
	NodeRoleTransition = 2
	NodeRoleBlocked    = 3
	NodeRoleProposed   = 4
)

func (o *NodeRole) Id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	}
	log.Panic("NodeRole has no ID")
	return ""
}

func (o *NodeRole) SetId(s string) error {
	if o.ID != 0 {
		return errors.New("SetId can only be used on an un-IDed object")
	}
	if id, err := strconv.ParseInt(s, 10, 64); err == nil {
		o.ID = id
	} else {
		return err
	}
	return nil
}

func (o *NodeRole) ApiName() string {
	return "node_roles"
}

func (o *NodeRole) setLastJSON(b []byte) {
	o.lastJson = make([]byte, len(b))
	copy(o.lastJson, b)
}

func (o *NodeRole) lastJSON() []byte {
	return o.lastJson
}

func (o *NodeRole) attribs() {}

type NodeRoler interface {
	Crudder
	nodeRoles()
}

func NodeRoles(scope ...NodeRoler) (res []*NodeRole, err error) {
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = url(scope[i])
	}
	res = make([]*NodeRole, 0)
	return res, session.list(&res, append(paths, "node_roles")...)
}
