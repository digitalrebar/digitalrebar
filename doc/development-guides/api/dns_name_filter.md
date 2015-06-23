### DNS Name Filter APIs

#### DNS Name Filter Routes

| Verb | URL | Options | Returns | Comments |
|:------|:-----------------------|--------|--------|:----------------|
| GET  | /api/v2/dns_name_filters  | none   | DNS Name Filter List | - | 
| POST | /api/v2/dns_name_filters  | none   | New DNS Name Filter | - | 
| GET  | /api/v2/dns_name_filters/[:id]  | none   | Existing DNS Name Filter Detail | - | 
| PUT  | /api/v2/dns_name_filters/[:id]  | none   | Update DNS Name Filter Detail | - | 
| DELETE  | /api/v2/dns_name_filters/[:id] | none   | Delete a DNS Name Filter | - | 


## JSON fields

|Attribute|Type|Settable|Note|
|---------|----|--------|----|
|id|Integer|No|Numeric id of filter|
|name|String|Yes|Descriptive name of the filter|
|priority|Integer|Yes|Unique priority to order evaluation of filters|
|matcher|String|Yes|A string that is used to match a network allocation|
|template|String|Yes|A string that is used to generate a DNS name for a matched node|
|system|String|Yes|A string that references a DNS Management Service to update|
|Created_at|String|No|Unicode - date format|
|Updated_at|String|No|Unicode - date format|

Minimum fields needed for create - name, priority, matcher, template, and system.

#### DNS Name Entry Routes

| Verb | URL | Options | Returns | Comments |
|:------|:-----------------------|--------|--------|:----------------|
| GET  | /api/v2/dns_name_entries  | none   | DNS Name Entry List | - | 
| GET  | /api/v2/dns_name_entries/[:id]  | none   | Existing DNS Name Entry Detail | - | 


## JSON fields

|Attribute|Type|Settable|Note|
|---------|----|--------|----|
|id|Integer|No|Numeric id of filter|
|name|String|No|DNS Name for the network allocation|
|rr_type|String|No|DNS RR Type of record|
|network_allocation|Integer|No|Id reference to network allocation|
|dns_name_filter|Integer|No|Id reference to the matching DNS Name Filter|
|Created_at|String|No|Unicode - date format|
|Updated_at|String|No|Unicode - date format|


