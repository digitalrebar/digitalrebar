package multitenancy

import (
	"encoding/json"
	"errors"
	"strconv"
	"net/http"
)

// Tenant tracks the capabilities the user making this request has
type Tenant struct {
	Parent       int      `json:"parent"`
	Capabilities []string `json:"capabilities"`
}

// CapabilityMap indexes Tenants by integer IDs
type CapabilityMap map[int]Tenant

// creates a new object capable of checking tenants for capabilities
func NewCapabilityMap(r *http.Request) (CapabilityMap, error) {
	// read the header that contains the cap map
	header := r.Header.Get("X-Authenticated-Capability")
	if header == "" {
		return nil, errors.New("No X-Authenticated-Capability Header")
	}

	// unmarshal raw api json
	var capMapJson map[string]Tenant
	capMap := CapabilityMap{}
	err := json.Unmarshal([]byte(header), &capMapJson)

	if err != nil {
		return nil, errors.New("Invalid Header JSON")
	}

	// convert string id keys to int keys
	for id, val := range capMapJson {
		i, _ := strconv.Atoi(id)
		capMap[i] = val
	}
	return capMap, nil
}

// Checks if a tenant has a specified capability
func (capMap CapabilityMap) HasCapability(tenantId int, cap string) bool {
	for tenantId > 0 { // if the tenant exists
		// check if the cap is in that tenant
		for _, c := range capMap[tenantId].Capabilities {
			if c == cap {
				return true
			}
		}

		// check the tenant's parent
		tenantId = capMap[tenantId].Parent

		// return if the tenant doesn't exist
		_, ok := capMap[tenantId]
		if !ok {
			return false
		}
	}

	return false
}
