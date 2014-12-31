### Role APIs

Roles are a core data type in OpenCrowbar.  They are used to define
services that OpenCrowbar deploys in the environment.

> Role names are globally unique. This restriction may be related in the future

#### Role Types

OpenCrowbar allows Barclamp creators to override the default Role
behavior.  This is a very import extension point for OpenCrowbar
because custom Role beaviors are essential to many orchestration
situations.

If no override is provided, OpenCrowbar will use the OpenCrowbar::Role class.

A role specific override can be created using the name of the
barclamp-role to create the class in the Barclamp model name space.
For example, a role called test-admin should be created as
BarclampTest::Admin (or app/models/barclamp_test/admin.rb).  When the
role is imported, OpenCrowbar will automatically use this type if it
has been defined.

A barclamp specific override can be created using the name of the
barclamp and the class role.  If present, this class will be used if
no specific role class has been provided.  This is very useful for
barclamps that create roles dynamically like the network barclamp.
For example, OpenCrowbar will use the BarclampNetwork::Role (or
app/models/barclamp_network/role.rb) class when new Network roles are
added.  This allows Barclamp creators to add custom event handling
without knowing the name of the roles in advance.

> This is also related to how Role Events are handled


#### API Actions

| Verb | URL | Comments |
|:-------|:-------------------|:----------------------|
| GET  | api/v2/roles | List |
| GET  | api/v2/roles/:id | Specific Item |
| GET  | /api/v2/roles/[:role_id]/attribs  |  List Attribs for a specific role|
| GET  | /api/v2/roles/[:role_id]/attribs/[:id]  |  Show Attrib (including value) for a specific Role|
| PUT  | /api/v2/roles/[:role_id]/attribs/[:id]  |  Update Attrib |
| PUT | - | NOT SUPPORTED / managed during import only |
| POST  | - | NOT SUPPORTED / roles are only created during import |
| DELETE  | - | NOT SUPPORTED |

#### Role Events (triggered on NodeRole state changes)

The Role model has a series of events (Self.on_[STATE]) that are
called when any NodeRole changes state.  This is a designed override
point where Barclamp Roles can add functionality into the OpenCrowbar
Annealer engine.  This functionality is added when a Barclamp defines
it's own Role definitions (Barclamp[Name]::[Role]).  If there is no
override, then the default behavior is used.

There is a matching event for each NodeRole state.  The event is
called when the NodeRole enters that state.  The purpose of this
function is to enable Barclamp creators to leverage information
available to OpenCrowbar within the Annealer operation including
before user editing (Proposed) or system error (Error) states.


## JSON fields

| Attribute | Type |Settable | Note
|:--------|:---------|:------|:-----------------------------|
|Description|String|Yes||
|Name|String|Yes||
|Created_at|String|No|Unicode - date format|
|Updated_at|String|No|Unicode - date format|
|Server|Boolean|Yes||
|Bootstrap|Boolean|Yes||
|Library|Boolean|Yes||
|Barclamp_id|Internal Ref|??|Actually an Int|
|Cluster|Boolean|Yes||
|Implicit|Boolean|Yes||
|Template|String|Yes|Another json blob|
|Jig Name|String|Yes||
|Destructive|Boolean|Yes||
|id|Internal Ref|??|Actually an Int|
|Discovery|Boolean|Yes||

### Structure of the template JSON (from an example)

The template structure is multi-layered - in the table assume that the
lines following a 'blob' are the subsidiary structure


|Name|Value
|:--------|:--------|
|Ceph|json blob|
|config|json blob|
|osd|json blob|
|journal|file|
|encrypt|False|
|fstype|xfs|





