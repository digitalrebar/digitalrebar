### Deployment-Role API

DeploymentRoles provide the default values for node-roles in a
deployment. They are populated from the role's template during import.

Unlike node-roles, they do not store any inbound or system data.

#### API Actions

| Verb | URL | Comments |
|:------|:-----------------------|:----------------|
| GET  | api/v2/deployment_roles | List |
| GET  | api/v2/deployment_roles/:id | Specific Item |
| PUT  | api/v2/deployment_roles/:id | Update Item |
| POST  | api/v2/deployment_roles | Create Item |
| GET  | /api/v2/deployment_roles/[:deployment_role_id]/attribs  |  List Attribs for a specific deployment_role |
| GET  | /api/v2/deployment_roles/[:deployment_role_id]/attribs/[:id]  | Show Attrib (including value) for a specific Deployment_Role |
| PUT  | /api/v2/deployment_roles/[:deployment_role_id]/attribs/[:id]  | Update Attrib |
| DELETE  | - | NOT SUPPORTED |
