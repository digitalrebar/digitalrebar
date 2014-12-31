### Barclamp APIs

Barclamps are the core modulization for OpenCrowbar.
For that reason, the API for barclamps is more limited because changes
to barclamps can cause breaking changes to the framework.

#### Routes

| Verb | URL | Options | Returns | Comments |
|:------|:-----------------------|--------|--------|:----------------|
| GET  | /api/v2/barclamps  | none   | List Barclamps | - | 
| GET  | /api/v2/barclamps/[:id]  | none   | Show Barclamp | - | 

#### List Barclamps

* CLI: `crowbar barclamps list`
* API: `curl -X GET
        --digest -u $(cat /etc/crowbar.install.key)
        -H "Content-Type:application/json"
        http://localhost:3000/api/v2/barclamps`

Returns:

    [
      {
        "name": "core",
        "source_url": "https://github.com/opencrowbar/core",
        "created_at": "2014-03-03T05:18:01.330Z",
        "id": 1,
        "source_path": "/opt/opencrowbar/core",
        "description": "Core",
        "commit": "unknown",
        "barclamp_id": 1,
        "version": 2,
        "build_on": null,
        "updated_at": "2014-03-03T05:18:01.400Z"
      },
      {
        "name": "crowbar",
        "source_url": "http://github/opencrowbar/unknown",
        "created_at": "2014-03-03T05:18:01.709Z",
        "id": 2,
        "source_path": "/opt/opencrowbar/core",
        "description": "Crowbar",
        "commit": "unknown",
        "barclamp_id": 2,
        "version": 2,
        "build_on": null,
        "updated_at": "2014-03-03T05:18:01.737Z"
      },
      ...
    ]

#### Show Barclamp

* CLI: `crowbar barclamps show core`
* API: `curl -X GET
        --digest -u $(cat /etc/crowbar.install.key)
        -H "Content-Type:application/json"
        http://localhost:3000/api/v2/barclamps/core`

Returns:

    {
      "commit": "unknown",
      "build_on": null,
      "updated_at": "2014-03-03T05:18:01.400Z",
      "description": "Core",
      "barclamp_id": 1,
      "version": 2,
      "source_url": "https://github.com/opencrowbar/core",
      "source_path": "/opt/opencrowbar/core",
      "created_at": "2014-03-03T05:18:01.330Z",
      "id": 1,
      "name": "core"
    }

