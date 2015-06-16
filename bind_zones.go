package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

type BindDnsInstance struct {
	dns_backend_point
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
func (di *BindDnsInstance) GetAllZones() ([]Zone, *backendError) {

	zones := di.findZones(nil)
	if zones == nil {
		return nil, &backendError{"File not available", 500}
	}
	return zones, nil
}

// Get function
func (di *BindDnsInstance) GetZone(id string) (Zone, *backendError) {
	zones := di.findZones(&id)
	if zones == nil || len(zones) == 0 {
		return Zone{}, &backendError{"Not Found", 404}
	}
	return zones[0], nil
}

// Create function
func (di *BindDnsInstance) PostZone(zone Zone) (Zone, *backendError) {
	// GREG: Create zone
	return zone, nil
}

// Update function
func (di *BindDnsInstance) PutZone(id string, zone Zone) (Zone, *backendError) {
	// GREG: update zone
	return zone, nil
}

// Delete function
func (di *BindDnsInstance) DeleteZone(id string) *backendError {
	// zone := di.parseZone(id)
	// GREG: remove zonze
	return nil
}

// Patch function
func (di *BindDnsInstance) PatchZone(id string, rrsets RRSets) (Zone, *backendError) {

	// GREG: Update zone info

	// GREG: reade in zone data

	var data Zone

	return data, nil
}
