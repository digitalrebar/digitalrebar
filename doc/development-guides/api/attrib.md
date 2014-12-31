### Attribute (aka Attrib) APIs

Attribute APIs are used to manage attributes used by the jigs.
Roles, Nodes, NodeRoles, and DeploymentRoles all work with Attribs.

> To prevent Rails name collisions, OpenCrowbar uses 'Attrib' instead of Attribute.

#### Routes

| Verb | URL | Options | Returns | Comments |
|:------|:-----------------------|--------|--------|:----------------|
| GET  | /api/v2/attribs  | none   | List Attribs | - | 
| GET  | /api/v2/attribs/[:id]  | none   | Show Attrib | - | 
| GET  | /api/v2/nodes/[:node_id]/attribs  | none   | List Attribs for a specific node| - | 
| GET  | /api/v2/nodes/[:node_id]/attribs/[:id]  | none   | Show Attrib (including value) for a specific Node| - | 
| PUT  | /api/v2/nodes/[:node_id]/attribs/[:id]  | none   | Update Attrib | - |
| GET  | /api/v2/roles/[:role_id]/attribs  | none   | List Attribs for a specific role| - | 
| GET  | /api/v2/roles/[:role_id]/attribs/[:id]  | none   | Show Attrib (including value) for a specific Role| - | 
| PUT  | /api/v2/roles/[:role_id]/attribs/[:id]  | none   | Update Attrib | - |
| GET  | /api/v2/deployments/[:deployment_id]/attribs  | none   | List Attribs for a specific deployment| - | 
| GET  | /api/v2/deployments/[:deployment_id]/attribs/[:id]  | none   | Show Attrib (including value) for a specific Deployment| - | 
| PUT  | /api/v2/deployments/[:deployment_id]/attribs/[:id]  | none   | Update Attrib | - |
| GET  | /api/v2/deployment_roles/[:deployment_role_id]/attribs  | none   | List Attribs for a specific deployment_role| - | 
| GET  | /api/v2/deployment_roles/[:deployment_role_id]/attribs/[:id]  | none   | Show Attrib (including value) for a specific Deployment_Role| - | 
| PUT  | /api/v2/deployment_roles/[:deployment_role_id]/attribs/[:id]  | none   | Update Attrib | - |
| GET  | /api/v2/node_roles/[:node_role_id]/attribs  | none   | List Attribs for a specific node_role| - | 
| GET  | /api/v2/node_roles/[:node_role_id]/attribs/[:id]  | none   | Show Attrib (including value) for a specific Node_Role| - | 
| PUT  | /api/v2/node_roles/[:node_role_id]/attribs/[:id]  | none   | Update Attrib | - |


#### List Attribs

* CLI: `crowbar attribs list`
* API: `curl -X GET
        --digest -u $(cat /etc/crowbar.install.key)
        -H "Content-Type:application/json"
        http://localhost:3000/api/v2/attribs`

Returns:

    [
      {
        "order": 10000,
        "barclamp_id": 2,
        "writable": false,
        "map": "ohai/dmi/base_board/asset_tag",
        "name": "asset_tag",
        "updated_at": "2014-03-03T05:18:01.883Z",
        "description": "BIOS configured system identifier",
        "id": 1,
        "role_id": null,
        "schema": null,
        "created_at": "2014-03-03T05:18:01.873Z"
      },
      {
        "order": 10000,
        "barclamp_id": 2,
        "writable": false,
        "map": "ohai/dmi/base_board/asset_tag",
        "name": "serial_number",
        "updated_at": "2014-03-03T05:18:01.909Z",
        "description": "System Serial Number",
        "id": 2,
        "role_id": null,
        "schema": null,
        "created_at": "2014-03-03T05:18:01.899Z"
      },
      ...
    ]

#### Show Attrib

* CLI: `crowbar attribs show hint-admin-macs`
* API: `curl -X GET
        --digest -u $(cat /etc/crowbar.install.key)
        -H "Content-Type:application/json"
        http://localhost:3000/api/v2/attribs/hint-admin/macs`

Returns

    {
      "writable": true,
      "map": "admin_macs",
      "created_at": "2014-03-03T05:18:02.241Z",
      "id": 14,
      "barclamp_id": 2,
      "description": "Hint for Admin MAC addresses",
      "order": 10000,
      "updated_at": "2014-03-03T05:18:02.254Z",
      "name": "hint-admin-macs",
      "schema": {
        "type": "seq",
        "sequence": [
          {
            "type": "str",
            "pattern": "/([0-9a-fA-F]{2}:){5}[0-9a-fA-F]/"
          }
        ],
        "required": true
      },
      "role_id": null
    }

