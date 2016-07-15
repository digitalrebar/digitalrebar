package multitenancy

import (
	"encoding/json"
	"errors"
	"github.com/ant0ine/go-json-rest/rest"
	"strconv"
)

type Tenant struct {
	Parent       int      `json:"parent"`
	Capabilities []string `json:"capabilities"`
}

type CapabilityMap map[int]Tenant

// creates a new object capable of checking tenants for capabilities
func NewCapabilityMap(r *rest.Request) (CapabilityMap, error) {
	// read the header that contains the cap map
	header := r.Header.Get("X-Authenticated-Capability")
	if header == "" {
		return nil, errors.New("No X-Authenticated-Capability Header")
	}

	// unmarshal raw api json
	var cap_map_json map[string]Tenant
	cap_map := CapabilityMap{}
	err := json.Unmarshal([]byte(header), &cap_map_json)

	if err != nil {
		return nil, errors.New("Invalid Header JSON")
	}

	// convert string id keys to int keys
	for id, val := range cap_map_json {
		i, _ := strconv.Atoi(id)
		cap_map[i] = val
	}
	return cap_map, nil
}

// Checks if a tenant has a specified capability
func (cap_map CapabilityMap) HasCapability(t_id int, cap string) bool {
	for t_id > 0 { // if the tenant exists
		// check if the cap is in that tenant
		for _, c := range cap_map[t_id].Capabilities {
			if c == cap {
				return true
			}
		}

		// check the tenant's parent
		t_id = cap_map[t_id].Parent

		// return if the tenant doesn't exist
		_, ok := cap_map[t_id]
		if !ok {
			return false
		}
	}

	return false
}
