package datatypes

// Provider represents a provisioning system that lets us create and delete Nodes
type Provider struct {
	NameID
	// Description is a human-readable description of this Provider
	Description string `json:"description"`
	// AuthDetails encapsulates low-level authentication information
	// for the Provider.
	AuthDetails map[string]interface{} `json:"auth_details"`
}

func (o *Provider) ApiName() string {
	return "providers"
}
