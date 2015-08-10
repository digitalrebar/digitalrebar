package datatypes

import "github.com/guregu/null"

// Deployments are the main tool that Crowbar provides to group
// related nodes together.  They carry Deployment-specific
// configuration information (in the form of DeploymentRoles), Nodes
// (a node belongs to exactly one Deployment at any given point in
// time), and NodeRoles (NodeRoles remember which Deployment they were
// first committed in, and always use that when calculating the attrib
// data to be passed in to a run).
//
// Deployments are structured as a tree with the system deployment at
// the root.  All newly-discovered Nodes start in the system
// deployment, where they have their inital set of Roles bound to
// them.
//
// Deployment satisfies the Attriber interface.  GetAttrib and
// SetAttrib calls on a Deployment will be redirected to the
// appropriate DeploymentRole in this Deployment.
type Deployment struct {
	NameID
	// The state a deployment is in
	State int `json:"state"`
	// A breif description of what the Deployment is for.
	Description string `json:"description"`
	// Whether the deployment is a system deployment.  Right now,
	// there can be only one of these.
	System bool `json:"system"`
	// The ID of the deployment that is the parent of this one.
	ParentID null.Int `json:"parent_id"`
}

func (o *Deployment) ApiName() string {
	return "deployments"
}

const (
	DeploymentError     = -1
	DeploymentProposed  = 0
	DeploymentCommitted = 1
	DeploymentActive    = 2
)

func (d *Deployment) Status() string {
	switch d.State {
	case DeploymentError:
		return "error"
	case DeploymentProposed:
		return "proposed"
	case DeploymentCommitted:
		return "committed"
	case DeploymentActive:
		return "active"
	}
	return "unknown"
}
