package datatypes

import "github.com/guregu/null"

// Jigs use Hammers to perform (hopefully) idempotent actions on Nodes
// on behalf of Roles through NodeRoles.
type Jig struct {
	NameID
	Description    string      `json:"description"`
	Active         bool        `json:"active"`
	ClientRoleName null.String `json:"client_role_name"`
	Server         null.String `json:"server"`
	ClientName     null.String `json:"client_name"`
	Key            null.String `json:"key"`
}

func (o *Jig) ApiName() string {
	return "jigs"
}
