package main

/* These are needed for database access
 *   "database/sql"
 *    _ "github.com/lib/pq"
 *
 * These are needed for consul api accces
 *    "github.com/hashicorp/consul/api"
 */

import (
	"encoding/json"
	"github.com/ant0ine/go-json-rest/rest"
	"io/ioutil"
	"log"
	"net/http"
)

type ZoneData struct {
	Entries map[string][]string // name -> [ip1, ip2, ip3]
}

type Frontend struct {
	Backend *dns_backend_point
	dns_frontend_point
	Zones map[string]ZoneData // zone name -> ZoneData
}

func (fe *Frontend) load_data(data_dir string) {
	bytes, err := ioutil.ReadFile(data_dir + "/database.json")
	if err != nil {
		log.Panic("failed to read file", err.Error())
	}

	err = json.Unmarshal(bytes, &fe.Zones)
	if err != nil {
		log.Panic("failed to parse file", err.Error())
	}
}

func (fe *Frontend) save_data(data_dir string) {
	jdata, err := json.Marshal(fe.Zones)
	if err != nil {
		log.Panic("Failed to marshal data", err.Error())
	}
	err = ioutil.WriteFile(data_dir+"/database.json", jdata, 0700)
	if err != nil {
		log.Panic("Failed to save data", err.Error())
	}
}

// List function
func (fe *Frontend) GetAllZones(w rest.ResponseWriter, r *rest.Request) {
	data, err := (*fe.Backend).GetAllZones()
	if err != nil {
		rest.Error(w, err.Error(), err.StatusCode())
		return
	}
	w.WriteJson(data)
}

// Get function
func (fe *Frontend) GetZone(w rest.ResponseWriter, r *rest.Request) {
	id := r.PathParam("id")

	data, err := (*fe.Backend).GetZone(id)
	if err != nil {
		rest.Error(w, err.Error(), err.StatusCode())
		return
	}
	w.WriteJson(data)
}

// Create function
func (fe *Frontend) PostZone(w rest.ResponseWriter, r *rest.Request) {
	zone := Zone{}
	err := r.DecodeJsonPayload(&zone)
	if err != nil {
		rest.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	data, berr := (*fe.Backend).PostZone(zone)
	if berr != nil {
		rest.Error(w, berr.Error(), berr.StatusCode())
		return
	}

	w.WriteJson(data)
}

// Update function
func (fe *Frontend) PutZone(w rest.ResponseWriter, r *rest.Request) {
	zone := Zone{}
	err := r.DecodeJsonPayload(&zone)
	if err != nil {
		rest.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	id := r.PathParam("id")

	data, derr := (*fe.Backend).PutZone(id, zone)
	if derr != nil {
		rest.Error(w, derr.Error(), derr.StatusCode())
		return
	}

	w.WriteJson(data)
}

// Delete function
func (fe *Frontend) DeleteZone(w rest.ResponseWriter, r *rest.Request) {
	id := r.PathParam("id")
	err := (*fe.Backend).DeleteZone(id)
	if err != nil {
		rest.Error(w, err.Error(), err.StatusCode())
	}
}

// Patch function
func (fe *Frontend) PatchZone(w rest.ResponseWriter, r *rest.Request) {
	rrsets := RRSets{}
	err := r.DecodeJsonPayload(&rrsets)
	if err != nil {
		rest.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	id := r.PathParam("id")

	data, derr := (*fe.Backend).PatchZone(id, rrsets)
	if derr != nil {
		rest.Error(w, derr.Error(), derr.StatusCode())
		return
	}

	w.WriteJson(data)
}
