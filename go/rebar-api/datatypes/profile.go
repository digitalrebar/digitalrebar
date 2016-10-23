package datatypes

type Profile struct {
	NameID
	TenantID int64                  `json:"tenant_id,omitempty"`
	Values   map[string]interface{} `json:"values"`
}

func (o *Profile) ApiName() string {
	return "profiles"
}
