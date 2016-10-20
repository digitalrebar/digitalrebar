package datatypes

// Node represents a system (real or virtual) that Rebar can manage.
type Node struct {
	NameID
	// The description of this Node.
	Description string `json:"description"`
	// Whether this node can coordinate and act on behalf of Rebar.
	// Any node that runs the Rebar API should have this set.
	Admin bool `json:"admin"`
	// Whether the node is powered on and reachable by Rebar.
	Alive bool `json:"alive"`
	// I do not remember what this flag is used for.
	Allocated bool `json:"allocated"`
	// Whether the node is available to the rest of the Rebar infrastructure.
	// Nodes that are not alive and available will not have jobs run on them.
	Available bool `json:"available"`
	// The current environment that the node should boot into.
	// This includes, but is not restricted to:
	//    * "local" for booting to local storage.
	//    * "<operating system>-install" for booting an OS install.
	//    * "sledgehammer" for booting to our discovery/inventory environment
	Bootenv string `json:"bootenv"`
	// Access IP
	CtrlAddr string `json:"node-control-address"`
	// The deployment that this Node is currently a member of.
	DeploymentID int64 `json:"deployment_id"`
	Order        int64 `json:"order"`
	// Whether this node is available to run non-system roles on.
	System bool `json:"system"`
	// The target Role that the annealer should converge the node to.
	// If it is 0, then the annealer will try to converge all the roles bound to the node.
	TargetRoleID int64    `json:"target_role_id"`
	Quirks       []string `json:"quirks"`
	// The variety of node this is.  Defaults to "metal", which is the only type we know
	// about right now.
	Variant string `json:"variant"`
	// The architecture of the node.  Roughly analogous to the ISA
	Arch string `json:"arch"`
	// The type of operating system the node is running. Should be something like
	// "linux", "freebsd", "windows", etc.
	OsFamily string `json:"os_family"`
	// The ID of the provider that is responsible for node provisioning.
	ProviderID int64 `json:"provider_id"`
	TenantID   int64 `json:"tenant_id,omitempty"`
}

func (o *Node) ApiName() string {
	return "nodes"
}
