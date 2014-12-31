## Deployment Role Model

### Hooks 

To integrate Deployment Roles, the following hooks can be overriden.

#### On Deployment Create

Used to create deployment scoped information when a role is added to to a deployment

Calls code immplemented on the attached Role

#### On Deployment Delete

Used to cleanup role data when it is removed from a deployment.

> At present, you cannot remove roles from a deployment.

Calls code immplemented on the attached Role.
