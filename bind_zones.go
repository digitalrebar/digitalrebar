package main

import (
	"bufio"
	"fmt"
	"github.com/ant0ine/go-json-rest/rest"
	"net/http"
	"os"
	"strings"
)

type BindDnsInstance struct {
	dns_endpoint
}

func (di *BindDnsInstance) parseZone(name string) *Zone {
	zone := Zone{
		Id:     name,
		Name:   name,
		Kind:   "Native",
		Dnssec: false,
	}

	filename := fmt.Sprintf("/etc/bind/db.%s", name)
	file, err := os.Open(filename)
	if err != nil {
		return nil
	}
	defer file.Close()

	// GREG: parse the template.

	return &zone
}

func (di *BindDnsInstance) findZones(id *string) []Zone {
	file, err := os.Open("/etc/bind/named.conf.crowbar")
	if err != nil {
		return nil
	}
	defer file.Close()

	zones := make([]Zone, 0, 10)
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		s := scanner.Text()

		if strings.HasPrefix(s, "include \"") {
			ss := strings.Split(s, "\"")
			zone := di.parseZone(strings.TrimPrefix(ss[1], "zone."))
			if zone != nil {
				zones = append(zones, *zone)
			}
		}
	}
	if err := scanner.Err(); err != nil {
		fmt.Fprintln(os.Stderr, "reading standard input:", err)
	}

	return zones
}

// List function
func (di *BindDnsInstance) GetAllZones(w rest.ResponseWriter, r *rest.Request) {

	zones := di.findZones(nil)
	if zones == nil {
		rest.Error(w, "File not available", 500)
		return
	}

	w.WriteJson(zones)
}

// Get function
func (di *BindDnsInstance) GetZone(w rest.ResponseWriter, r *rest.Request) {
	id := r.PathParam("id")

	zones := di.findZones(&id)
	if zones == nil || len(zones) == 0 {
		rest.Error(w, "Not Found", 404)
		return
	}

	w.WriteJson(zones[0])
}

// Create function
func (di *BindDnsInstance) PostZone(w rest.ResponseWriter, r *rest.Request) {
	zone := Zone{}
	err := r.DecodeJsonPayload(&zone)
	if err != nil {
		rest.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	// GREG: Create zone

	w.WriteJson(zone)
}

// Update function
func (di *BindDnsInstance) PutZone(w rest.ResponseWriter, r *rest.Request) {
	zone := Zone{}
	err := r.DecodeJsonPayload(&zone)
	if err != nil {
		rest.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	// GREG: update zone

	w.WriteJson(zone)
}

// Delete function
func (di *BindDnsInstance) DeleteZone(w rest.ResponseWriter, r *rest.Request) {
	id := r.PathParam("id")
	zone := di.parseZone(id)
	// GREG: remove zonze
	w.WriteJson(zone)
}

// Patch function
func (di *BindDnsInstance) PatchZone(w rest.ResponseWriter, r *rest.Request) {
	rrsets := RRSets{}
	err := r.DecodeJsonPayload(&rrsets)
	if err != nil {
		rest.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	// GREG: Update zone info

	// GREG: reade in zone data

	var data Zone

	w.WriteJson(data)
}
