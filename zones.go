package main

import (
	"encoding/json"
	"github.com/ant0ine/go-json-rest/rest"
	"io/ioutil"
	"log"
	"net/http"
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
type ZoneData map[string][]ZoneEntry   // name -> [{}, {}, {}]
type ZoneTrackers map[string]*ZoneData // zone name -> ZoneData

/*
 * Structure for the front end with a pointer to the backend
 */
type Frontend struct {
	dns_frontend_point
	Backend  *dns_backend_point
	Zones    *ZoneTrackers
	data_dir string
}

/*
 * Data storage/retrieval functions
 */
func (fe *Frontend) load_data() {
	bytes, err := ioutil.ReadFile(fe.data_dir + "/database.json")
	if err != nil {
		log.Panic("failed to read file", err.Error())
	}

	err = json.Unmarshal(bytes, &fe.Zones)
	if err != nil {
		log.Panic("failed to parse file", err.Error())
	}
}

func (fe *Frontend) save_data() {
	jdata, err := json.Marshal(fe.Zones)
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
	data, err := (*fe.Backend).GetAllZones(fe.Zones)
	if err != nil {
		rest.Error(w, err.Error(), err.StatusCode())
		return
	}
	w.WriteJson(data)
}

// Get function
func (fe *Frontend) GetZone(w rest.ResponseWriter, r *rest.Request) {
	id := r.PathParam("id")

	data, err := (*fe.Backend).GetZone(fe.Zones, id)
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

	id := r.PathParam("id")

	// GREG: Mutex Start by name
	zone := (*fe.Zones)[id]
	if zone == nil {
		zone = &ZoneData{}
		(*fe.Zones)[id] = zone
	}

	if record.ChangeType == "ADD" {
		zes := (*zone)[record.Name]
		// Make holder for name if not there
		if zes == nil {
			(*zone)[record.Name] = make([]ZoneEntry, 0, 10)
			zes = (*zone)[record.Name]
		}

		// Check if data already exists, return if so
		for _, ze := range zes {
			if ze.Content == record.Content && ze.Type == record.Type {
				// GREG: Return something
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
		(*zone)[record.Name] = append((*zone)[record.Name], nze)
		fe.save_data()
	} else if record.ChangeType == "REMOVE" {
		zes := (*zone)[record.Name]
		if zes != nil {
			for i, ze := range zes {
				// Remove the entry from the slice
				if ze.Content == record.Content && ze.Type == record.Type {
					zes[i], (*zone)[record.Name] = zes[len(zes)-1], zes[:len(zes)-1]
					fe.save_data()
					break
				}
			}
		}
	} else {
		// GREG: Invalid data
		// GREG: Mutex Stop by name or validate before starting.
		rest.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

output:
	// GREG: Mutex Stop by name

	data, derr := (*fe.Backend).PatchZone(id, record.Name, zone)
	if derr != nil {
		rest.Error(w, derr.Error(), derr.StatusCode())
		return
	}

	w.WriteJson(data)
}
