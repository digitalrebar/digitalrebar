package datatypes

// DeploymentRoles represent a binding of a Role to a Deployment.
// They are where the deployment-specific configuration information
// for Roles are stored (as Attribs on the DeploymentRole).
// DeploymentRoles have the following attrib buckets:
//
//    * proposed
//    * committed
//    * system
//
// DeploymentRole satisfies the Attriber interface.
type DeploymentRole struct {
	SimpleID
	DeploymentID int64 `json:"deployment_id"`
	RoleID       int64 `json:"role_id"`
	TenantID     int64 `json:"tenant_id"`
}

func (o *DeploymentRole) ApiName() string {
	return "deployment_roles"
}
