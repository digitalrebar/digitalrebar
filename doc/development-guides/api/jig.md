### Jig (aka CMDB interface) APIs

Jigs are the interface between OpenCrowbar and doing work in the infrastructure.

#### System Jigs

OpenCrowbar has three built-in jigs

* Script - uses SSH to perform operations on nodes.  This is used for bootstrapping actions that install the agents for other Jigs.  Not activated in development mode.
* Noop (no operation) - takes internal actions in OpenCrowbar only.  Used when database updates or coordination points are needed that have no external action.
* Test - used by the test infrastructure to validate OpenCrowbar logic when no phyiscal infrastructure is available.  Not activited in production mode.

#### API Actions

| Verb | URL | Comments |
|:------|:-----------------------|:----------------|
| GET  |api/v2/jigs |List |
| GET  |api/v2/jigs/:id |Specific Item |
| PUT  |api/v2/jigs/:id |Update Item |
| POST  |api/v2/jigs |Create Item |
| DELETE  |api/v2/jigs/:id |Delete Item |
| VARIOUS  |api/v2/jigs/:id/extra |Special Ops |
