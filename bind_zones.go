package main

import (
	"bufio"
	"fmt"
	"github.com/ant0ine/go-json-rest/rest"
	"log"
	"net/http"
	"os"
	"strings"
)

func (di *BindDnsInstance) parseZone(name string) Zone {
	return Zone{}
}

// List function
func (di *BindDnsInstance) GetAllZones(w rest.ResponseWriter, r *rest.Request) {

	file, err := os.Open("/etc/bind/named.conf.crowbar")
	if err != nil {
		// GREG: write an error back the caller
		log.Fatal(err)
	}
	defer file.Close()

	zones := make([]Zone, 0, 10)
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		s := scanner.Text()

		if strings.HasPrefix(s, "include \"") {
			ss := strings.Split(s, "\"")
			zone := di.parseZone(ss[1])
			zones = append(zones, zone)
		}
	}
	if err := scanner.Err(); err != nil {
		fmt.Fprintln(os.Stderr, "reading standard input:", err)
	}

	w.WriteJson(zones)
}

// Get function
func (di *BindDnsInstance) GetZone(w rest.ResponseWriter, r *rest.Request) {
	id := r.PathParam("id")

	zone := di.parseZone(id)

	w.WriteJson(zone)
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
