### DHCP Database

This special read only Provisioner API is used to manage expose DHCP Entries.

Lists the dhcp entries.

> The testing system is the intended consumer of this API

| Verb | URL | Options | Returns | Comments |
|:------|:-----------------------|--------|--------|:----------------|
| GET  | api/v2/dhcp | N/A | JSON array of DHCP clients | |
| GET  | api/v2/dhcp/[node] | Information about the requested client only | Provided as convenience for testing | |
