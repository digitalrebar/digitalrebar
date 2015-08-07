package datatypes

// Node represents a system (real or virtual) that Crowbar can manage.
type Node struct {
	NameID
	// The description of this Node.
	Description string `json:"description"`
	// Whether this node can coordinate and act on behalf of Crowbar.
	// Any node that runs the Crowbar API should have this set.
	Admin bool `json:"admin"`
	// Whether the node is powered on and reachable by Crowbar.
	Alive bool `json:"alive"`
	// I do not remember what this flag is used for.
	Allocated bool `json:"allocated"`
	// Whether the node is available to the rest of the Crowbar infrastructure.
	// Nodes that are not alive and available will not have jobs run on them.
	Available bool `json:"available"`
	// The current environment that the node should boot into.
	// This includes, but is not restricted to:
	//    * "local" for booting to local storage.
	//    * "<operating system>-install" for booting an OS install.
	//    * "sledgehammer" for booting to our discovery/inventory environment
	Bootenv string `json:"bootenv"`
	// The deployment that this Node is currently a member of.
	DeploymentID int64 `json:"deployment_id"`
	Order        int64 `json:"order"`
	// Whether this node is available to run non-system roles on.
	System bool `json:"system"`
	// The target Role that the annealer should converge the node to.
	// If it is 0, then the annealer will try to converge all the roles bound to the node.
	TargetRoleID int64 `json:"target_role_id"`
}

func (o *Node) ApiName() string {
	return "nodes"
}
