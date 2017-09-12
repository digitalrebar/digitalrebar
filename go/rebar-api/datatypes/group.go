package datatypes

type Group struct {
	NameID
	Description string `json:"description"`
	Category    string `json:"category"`
	Order       int64  `json:"order"`
	TenantID    int64  `json:"tenant_id,omitempty"`
}

func (o *Group) ApiName() string {
	return "groups"
}
