package datatypes

import "path"

type UserTenantCapability struct {
	SimpleID
	UserID       int64 `json:"user_id"`
	TenantID     int64 `json:"tenant_id"`
	CapabilityID int64 `json:"capability_id"`
}

func (o *UserTenantCapability) ApiName() string {
	return "user_tenant_capabilities"
}

func (o *UserTenantCapability) ApiPath() string {
	return path.Join(API_PATH, o.ApiName())
}
