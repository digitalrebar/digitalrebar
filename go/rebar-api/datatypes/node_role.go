package datatypes

import "path"

// NodeRole represents the binding of a Role to a Node in a Deployment.
// All of them together are linked in a graph that defines how everything
// in the cluster is related.
type NodeRole struct {
	SimpleID
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
	Available bool  `json:"available"`
	Order     int   `json:"order"`
	NodeError bool  `json:"node_error"`
	Cohort    int64 `json:"cohort"`
	RunCount  int64 `json:"run_count"`
	TenantID  int64 `json:"tenant_id,omitempty"`
}

func (o *NodeRole) ApiName() string {
	return "node_roles"
}

func (o *NodeRole) ApiPath() string {
	return path.Join(API_PATH, o.ApiName())
}

const (
	NodeRoleError      = -1
	NodeRoleActive     = 0
	NodeRoleTodo       = 1
	NodeRoleTransition = 2
	NodeRoleBlocked    = 3
	NodeRoleProposed   = 4
)

func (o *NodeRole) StateName() string {
	switch o.State {
	case NodeRoleError:
		return "error"
	case NodeRoleActive:
		return "active"
	case NodeRoleTodo:
		return "todo"
	case NodeRoleTransition:
		return "transition"
	case NodeRoleBlocked:
		return "blocked"
	case NodeRoleProposed:
		return "proposed"
	}
	return "unknown"
}
