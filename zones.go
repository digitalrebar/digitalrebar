package main

import (
	"log"
	"net/http"
	"sync"
	"strconv"

	"github.com/ant0ine/go-json-rest/rest"
	"github.com/digitalrebar/go-common/multi-tenancy"
)

/*
 * Managment API Structures
 *
 * These are the front end api structures.
 * They are similar to the PowerDNS structures, but
 * are different.  The RRSet semantic in PowerDNS
 * is replace/delete.  The management layer is just
 * add/or delete.  The set math is done in this application
 * to figure out the actual resulting set of data to
 * write to the backend for the key (name).
 *
 * These match the json objects that are needed to
 * update/create and get zone information and records
 */
type Zone struct {
	Name    string   `json:"name"`
	Records []Record `json:"records,omitempty"`
	TenantId int     `json:"tenant_id"`
}

type Record struct {
	ChangeType string `json:"changetype"` // ADD or REMOVE
	Content    string `json:"content"`
	Name       string `json:"name"`
	Type       string `json:"type"`
}

/*
 * Internal data storage to track adds/removes
 *
 * Bind needs this to build the complete bind zone files.
 * PDNS needs this to build aggregate requests
 */
type ZoneContent struct {
	Content string // IPv4, IPv6, name
}
type ZoneEntry struct {
	Types map[string][]ZoneContent // type -> [{}, {}, {}]
}

func NewZoneEntry() *ZoneEntry {
	return &ZoneEntry{
		Types: make(map[string][]ZoneContent),
	}
}

type ZoneData struct {
	Entries map[string]*ZoneEntry // name -> entry
	TenantId int
}

func NewZoneData() *ZoneData {
	return &ZoneData{
		Entries: make(map[string]*ZoneEntry),
	}
}

type ZoneTracker struct {
	sync.Mutex `json:"-"`
	Zones      map[string]*ZoneData // zone name -> ZoneData
}

func NewZoneTracker() *ZoneTracker {
	return &ZoneTracker{
		Zones: make(map[string]*ZoneData),
	}
}

/*
 * Structure for the front end with a pointer to the backend
 */
type Frontend struct {
	dns_frontend_point
	Backend  *dns_backend_point
	ZoneInfo *ZoneTracker
	store    LoadSaver
}

func NewFrontend(backend *dns_backend_point, store LoadSaver) *Frontend {
	return &Frontend{
		Backend:  backend,
		ZoneInfo: NewZoneTracker(),
		store:    store,
	}
}

/*
 * Data storage/retrieval functions
 */
func (fe *Frontend) load_data() {
	if err := fe.store.Load(fe.ZoneInfo); err != nil {
		log.Panic(err)
	}
}

func (fe *Frontend) save_data() {
	if err := fe.store.Save(fe.ZoneInfo); err != nil {
		log.Panic(err)
	}

}

// List function
func (fe *Frontend) GetAllZones(w rest.ResponseWriter, r *rest.Request) {
	data, err := (*fe.Backend).GetAllZones(fe.ZoneInfo)
	if err != nil {
		rest.Error(w, err.Error(), err.StatusCode())
		return
	}
	capMap, _ := multitenancy.NewCapabilityMap(r.Request)
	zones := make([]Zone, 0, len(data))
	for _, zone := range(data) {
		if capMap.HasCapability(zone.TenantId, "ZONE_READ") {
			zones = append(zones, zone)
		}
	}
	w.WriteJson(zones)
}

// Get function
func (fe *Frontend) GetZone(w rest.ResponseWriter, r *rest.Request) {
	zoneName := r.PathParam("id")

	data, err := (*fe.Backend).GetZone(fe.ZoneInfo, zoneName)
	if err != nil {
		rest.Error(w, err.Error(), err.StatusCode())
		return
	}
	capMap, _ := multitenancy.NewCapabilityMap(r.Request)
	if capMap.HasCapability(data.TenantId, "ZONE_READ") {
		w.WriteJson(data)
	} else {
		rest.Error(w, "Not Found", http.StatusNotFound)
	}
}

// Patch function
func (fe *Frontend) PatchZone(w rest.ResponseWriter, r *rest.Request) {
	record := Record{}
	err := r.DecodeJsonPayload(&record)
	if err != nil {
		rest.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	zoneName := r.PathParam("id")
	tenantId, _ := strconv.Atoi(r.PathParam("tenant_id"))

	capMap, _ := multitenancy.NewCapabilityMap(r.Request)
	fe.ZoneInfo.Lock()
	zone := fe.ZoneInfo.Zones[zoneName]
	if zone != nil {
		tenantId = zone.TenantId
	}
	if zone != nil || !capMap.HasCapability(tenantId, "ZONE_READ") || !capMap.HasCapability(tenantId, "ZONE_UPDATE"){
		if !capMap.HasCapability(tenantId, "ZONE_READ") {
			rest.Error(w, "Not Found", http.StatusNotFound)
		} else {
			rest.Error(w, "Forbidden", http.StatusForbidden)
		}
		return
	}
	switch record.ChangeType {
	case "ADD":
		// If no zone, create zone
		if zone == nil {
			zone = NewZoneData()
			zone.TenantId = tenantId
			fe.ZoneInfo.Zones[zoneName] = zone
		}

		// If no entry, create entry
		zes := zone.Entries[record.Name]
		// Make holder for name if not there
		if zes == nil {
			zone.Entries[record.Name] = NewZoneEntry()
			zes = zone.Entries[record.Name]
		}

		// Check if type exists, if not create it
		zt := zes.Types[record.Type]
		if zt == nil {
			zt = make([]ZoneContent, 0, 2)
			zes.Types[record.Type] = zt
		}

		// Check the list for content
		for _, ze := range zt {
			if ze.Content == record.Content {
				// Already have data. Just return
				goto output
			}
		}

		// Add new entry
		nze := ZoneContent{
			Content: record.Content,
		}
		zes.Types[record.Type] = append(zt, nze)
		fe.save_data()
	case "REMOVE":
		if zone == nil {
			goto output
		}
		zes := zone.Entries[record.Name]
		if zes == nil {
			goto output
		}
		zt := zes.Types[record.Type]
		if zt == nil {
			goto output
		}

		for i, zc := range zt {
			// Remove the entry from the slice
			if zc.Content == record.Content {
				zt[i], zes.Types[record.Type] = zt[len(zt)-1], zt[:len(zt)-1]
				if len(zes.Types[record.Type]) == 0 {
					delete(zes.Types, record.Type)
				}
				if len(zes.Types) == 0 {
					delete(zone.Entries, record.Name)
				}
				if len(zone.Entries) == 0 {
					delete(fe.ZoneInfo.Zones, zoneName)
				}
				fe.save_data()
				break
			}
		}
	default:
		fe.ZoneInfo.Unlock()
		rest.Error(w, "Invalid action type", 400)
		return
	}

output:
	fe.ZoneInfo.Unlock()

	data, derr := (*fe.Backend).PatchZone(fe.ZoneInfo, zoneName, record)
	if derr != nil {
		rest.Error(w, derr.Error(), derr.StatusCode())
		return
	}

	w.WriteJson(data)
}
