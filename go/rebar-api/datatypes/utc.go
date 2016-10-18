package datatypes

type UserTenantCapability struct {
	SimpleID
	UserID       int64 `json:"user_id"`
	TenantID     int64 `json:"tenant_id"`
	CapabilityID int64 `json:"capability_id"`
}

func (o *UserTenantCapability) ApiName() string {
	return "user_tenant_capabilities"
}
