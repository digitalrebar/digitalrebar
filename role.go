package crowbar

import (
	"errors"
	"log"
	"strconv"
)

// Role is a discrete unit of functionality that can be run on or
// against a Node.  If you want to install, configure, or monitor
// something on a Node, a Role is what you need to write to
// encapsulate that functionality.  Roles depend on one another both
// directly (forming the role graph), and indirectly (via providing
// and wanting Attribs).
type Role struct {
	// The ID of the Role.  This is an opaque integer thst is
	// unique among all Roles.
	ID int64 `json:"id,omitempty"`
	// The name of the Role.  This must also be unique amongst all
	// the Roles.
	Name string `json:"name,omitempty" yaml:"name,omitempty"`
	// A description of the Role.
	Description string `json:"description,omitempty" yaml:"name,omitempty"`
	// The Barclamp that the Role is a member of.  Barclamps are
	// collections of related Roles that collectively implement a
	// full workload.
	BarclampID int64 `json:"barclamp_id,omitempty"`
	// The name of the Jig that is responsible for performing
	// whatever actions this Role needs to perform on a Node.
	// Things like Chef, Salt, and Ansible provide jigs.  Jigs
	// must be idempotent.
	JigName string `json:"jig_name,omitempty" yaml:"jig_name,omitempty"`
	// Whether this Role needs to be bound to a Node to work.
	// Roles that exist only tp provide non node specific
	// configuration information are Abstract.
	Abstract bool `json:"abstract,omitempty"`
	// Whether this role is needed to bootstrap a working Crowbar cluster.
	Bootstrap bool `json:"bootstrap,omitempty"`
	// Whether this role should implement default clustering
	// behaviour.  In Crowbar terms, this flag changes how a
	// NodeRole binding for this Role gets bound into the noderole
	// graph to ensure that all nodes with this Role in the same
	// deployment become Active before allowing any downstream
	// NodeRoles in the graph to anneal.
	Cluster bool `json:"cluster,omitempty"`
	// Destructive indicates that this Role is not itempotent, and
	// should not be rerun after it has successfuly run once.
	Destructive bool `json:"destructive,omitempty"`
	// Discovery indicates that this Role should be bound to nodes
	// when they are discovered.
	Discovery bool `json:"discovery,omitempty"`
	// Implicit indicates that this noderole must be bound to the
	// same node as its parents.
	Implicit bool `json:"implicit,omitempty"`
	// Library is deprecated.
	Library bool `json:"library,omitempty"`
	// Milestone indicates that this Role is important enough to
	// be shown in the UI by default.
	Milestone bool `json:"milestone,omitempty"`
	// Powersave indicates that Crowbar may power the node off
	// after a NodeRole binding with this node has transitioned to
	// Active and there are no children of the NodeRole in the
	// node role graph.
	Powersave bool `json:"powersave,omitempty"`
	// Service indicates that this Role is a proxy for an external
	// service that Crowbar should rely on.
	Service bool `json:"service,omitempty"`
	// Cohort is the maximum number of hops there is between this
	// role and a root in the role graph.  It is used for ordering
	// purposes internally.
	Cohort    int      `json:"cohort,omitempty"`
	Conflicts []string `json:"conflicts,omitempty" yaml:"conflicts,omitempty"`
	Provides  []string `json:"provides,omitempty" yaml:"provides,omitempty"`
	CreatedAt string   `json:"created_at,omitempty"`
	UpdatedAt string   `json:"updated_at,omitempty"`
}

func (o *Role) Id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	} else if o.Name != "" {
		return o.Name
	} else {
		log.Panic("Role has no ID or name")
		return ""
	}
}

func (o *Role) SetId(s string) error {
	if o.ID != 0 || o.Name != "" {
		return errors.New("SetId can only be used on an un-IDed object")
	}
	if id, err := strconv.ParseInt(s, 10, 64); err == nil {
		o.ID = id
	} else {
		o.Name = s
	}
	return nil
}

func (o *Role) ApiName() string {
	return "roles"
}

func (o *Role) Match() (res []*Role, err error) {
	res = make([]*Role, 0)
	return res, session.match(o, &res, o.ApiName(), "match")
}

func (o *Role) attribs()         {}
func (o *Role) nodes()           {}
func (o *Role) deployments()     {}
func (o *Role) deploymentRoles() {}
func (o *Role) nodeRoles()       {}

type Roler interface {
	Crudder
	roles()
}

func Roles(scope ...Roler) (res []*Role, err error) {
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = url(scope[i])
	}

	res = make([]*Role, 0)
	return res, session.list(&res, append(paths, "roles")...)
}
