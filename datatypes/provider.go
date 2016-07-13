package datatypes

// Provider represents a provisioning system that lets us create and delete Nodes
type Provider struct {
	NameID
	// Description is a human-readable description of this Provider
	Description string `json:"description"`
	// AuthDetails encapsulates low-level authentication information
	// for the Provider.
	AuthDetails map[string]interface{} `json:"auth_details"`
	// Type descripes what type of provider this is.  Among other things, it is used
	// by the API to figure out how calls to it should be handled.
	Type     string `json:"type"`
	TenantID int64  `json:"tenant_id"`
}

func (o *Provider) ApiName() string {
	return "providers"
}
