package main

import (
	"encoding/json"
	"github.com/ant0ine/go-json-rest/rest"
	"io/ioutil"
	"log"
	"net/http"
	"sync"
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
}

type Record struct {
	ChangeType string `json:"changetype"` // ADD or REMOVE
	Content    string `json:"content"`
	Name       string `json:"name"`
	TTL        int    `json:"ttl"`
	Type       string `json:"type"`
}

/*
 * Internal data storage to track adds/removes
 *
 * Bind needs this to build the complete bind zone files.
 * PDNS needs this to build aggregate requests
 */
type ZoneEntry struct {
	Content string // IPv4, IPv6, name
	Type    string // A,    AAAA, CNAME
	TTL     int
}
type ZoneData struct {
	Entries map[string][]ZoneEntry // name -> [{}, {}, {}]
}

func NewZoneData() *ZoneData {
	return &ZoneData{
		Entries: make(map[string][]ZoneEntry),
	}
}

type ZoneTracker struct {
	Zones map[string]*ZoneData // zone name -> ZoneData
	Lock  sync.Mutex           `json:"-"`
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
	data_dir string
}

func NewFrontend(backend *dns_backend_point, data_dir string) *Frontend {
	return &Frontend{
		Backend:  backend,
		ZoneInfo: NewZoneTracker(),
		data_dir: data_dir,
	}
}

/*
 * Data storage/retrieval functions
 */
func (fe *Frontend) load_data() {
	bytes, err := ioutil.ReadFile(fe.data_dir + "/database.json")
	if err != nil {
		log.Panic("failed to read file", err.Error())
	}

	err = json.Unmarshal(bytes, &fe.ZoneInfo)
	if err != nil {
		log.Panic("failed to parse file", err.Error())
	}
}

func (fe *Frontend) save_data() {
	jdata, err := json.Marshal(fe.ZoneInfo)
	if err != nil {
		log.Panic("Failed to marshal data", err.Error())
	}
	err = ioutil.WriteFile(fe.data_dir+"/database.json", jdata, 0700)
	if err != nil {
		log.Panic("Failed to save data", err.Error())
	}
}

// List function
func (fe *Frontend) GetAllZones(w rest.ResponseWriter, r *rest.Request) {
	data, err := (*fe.Backend).GetAllZones(fe.ZoneInfo)
	if err != nil {
		rest.Error(w, err.Error(), err.StatusCode())
		return
	}
	w.WriteJson(data)
}

// Get function
func (fe *Frontend) GetZone(w rest.ResponseWriter, r *rest.Request) {
	zoneName := r.PathParam("id")

	data, err := (*fe.Backend).GetZone(fe.ZoneInfo, zoneName)
	if err != nil {
		rest.Error(w, err.Error(), err.StatusCode())
		return
	}
	w.WriteJson(data)
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

	fe.ZoneInfo.Lock.Lock()
	zone := fe.ZoneInfo.Zones[zoneName]
	if record.ChangeType == "ADD" {
		if zone == nil {
			zone = NewZoneData()
			fe.ZoneInfo.Zones[zoneName] = zone
		}

		zes := zone.Entries[record.Name]
		// Make holder for name if not there
		if zes == nil {
			zone.Entries[record.Name] = make([]ZoneEntry, 0, 10)
			zes = zone.Entries[record.Name]
		}

		// Check if data already exists, return if so
		for _, ze := range zes {
			if ze.Content == record.Content && ze.Type == record.Type {
				// Already have data. Just return
				goto output
			}
		}

		// Add new entry
		nze := ZoneEntry{
			Content: record.Content,
			Type:    record.Type,
			TTL:     record.TTL,
		}
		zone.Entries[record.Name] = append(zes, nze)
		fe.save_data()
	} else if record.ChangeType == "REMOVE" {
		if zone == nil {
			goto output
		}
		zes := zone.Entries[record.Name]
		if zes != nil {
			for i, ze := range zes {
				// Remove the entry from the slice
				if ze.Content == record.Content && ze.Type == record.Type {
					zes[i], zone.Entries[record.Name] = zes[len(zes)-1], zes[:len(zes)-1]
					if len(zone.Entries[record.Name]) == 0 {
						delete(zone.Entries, record.Name)
					}
					if len(zone.Entries) == 0 {
						delete(fe.ZoneInfo.Zones, zoneName)
					}
					fe.save_data()
					break
				}
			}
		}
	} else {
		fe.ZoneInfo.Lock.Unlock()
		rest.Error(w, "Invalid action type", 400)
		return
	}

output:
	fe.ZoneInfo.Lock.Unlock()

	data, derr := (*fe.Backend).PatchZone(fe.ZoneInfo, zoneName, record)
	if derr != nil {
		rest.Error(w, derr.Error(), derr.StatusCode())
		return
	}

	w.WriteJson(data)
}
