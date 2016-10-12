package datatypes

import "path"

// Role is a discrete unit of functionality that can be run on or
// against a Node.  If you want to install, configure, or monitor
// something on a Node, a Role is what you need to write to
// encapsulate that functionality.  Roles depend on one another both
// directly (forming the role graph), and indirectly (via providing
// and wanting Attribs).
type Role struct {
	NameID
	// A description of the Role.
	Description string `json:"description"`
	// The Barclamp that the Role is a member of.  Barclamps are
	// collections of related Roles that collectively implement a
	// full workload.
	BarclampID int64 `json:"barclamp_id"`
	// The name of the Jig that is responsible for performing
	// whatever actions this Role needs to perform on a Node.
	// Things like Chef, Salt, and Ansible provide jigs.  Jigs
	// must be idempotent.
	JigName string `json:"jig_name"`
	// Whether this Role needs to be bound to a Node to work.
	// Roles that exist only tp provide non node specific
	// configuration information are Abstract.
	Abstract bool `json:"abstract"`
	// Whether this role is needed to bootstrap a working Rebar cluster.
	Bootstrap bool `json:"bootstrap"`
	// Whether this role should implement default clustering
	// behaviour.  In Rebar terms, this flag changes how a
	// NodeRole binding for this Role gets bound into the noderole
	// graph to ensure that all nodes with this Role in the same
	// deployment become Active before allowing any downstream
	// NodeRoles in the graph to anneal.
	Cluster bool `json:"cluster"`
	// Destructive indicates that this Role is not itempotent, and
	// should not be rerun after it has successfuly run once.
	Destructive bool `json:"destructive"`
	// Discovery indicates that this Role should be bound to nodes
	// when they are discovered.
	Discovery bool `json:"discovery"`
	// Implicit indicates that this noderole must be bound to the
	// same node as its parents.
	Implicit bool `json:"implicit"`
	// Library is deprecated.
	Library bool `json:"library"`
	// Milestone indicates that this Role is important enough to
	// be shown in the UI by default.
	Milestone bool `json:"milestone"`
	// Powersave indicates that Rebar may power the node off
	// after a NodeRole binding with this node has transitioned to
	// Active and there are no children of the NodeRole in the
	// node role graph.
	Powersave bool `json:"powersave"`
	// Service indicates that this Role is a proxy for an external
	// service that Rebar should rely on.
	Service bool `json:"service"`
	// Cohort is the maximum number of hops there is between this
	// role and a root in the role graph.  It is used for ordering
	// purposes internally.
	Cohort    int      `json:"cohort"`
	Conflicts []string `json:"conflicts"`
	Provides  []string `json:"provides"`
	// Metadata is an structure passed to the jib for operation
	Metadata interface{} `json:"metadata"`
}

func (o *Role) ApiName() string {
	return "roles"
}

func (o *Role) ApiPath() string {
	return path.Join(API_PATH, o.ApiName())
}
