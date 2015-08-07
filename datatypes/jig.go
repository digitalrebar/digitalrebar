package datatypes

// Jigs use Hammers to perform (hopefully) idempotent actions on Nodes
// on behalf of Roles through NodeRoles.
type Jig struct {
	NameID
	Description    string `json:"description"`
	Active         bool   `json:"active"`
	ClientRoleName string `json:"client_role_name"`
	Server         string `json:"server"`
	ClientName     string `json:"client_name"`
	Key            string `json:"key"`
}

func (o *Jig) ApiName() string {
	return "jigs"
}
