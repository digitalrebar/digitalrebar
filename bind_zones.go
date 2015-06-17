package main

import (
	"bytes"
	"fmt"
	"net"
	"net/http"
	"os"
	"os/exec"
	"text/template"
)

type BindDnsInstance struct {
	dns_backend_point
	Templates  *template.Template
	ServerName string
}

func buildZone(name string, zoneData *ZoneData) Zone {

	records := make([]Record, 0, 100)

	if zoneData != nil {
		for name, entry := range zoneData.Entries {
			for t, contents := range entry.Types {
				for _, content := range contents {
					record := Record{
						Name:    name,
						Content: content.Content,
						Type:    t,
					}
					records = append(records, record)
				}
			}
		}
	}

	zone := Zone{
		Name:    name,
		Records: records,
	}

	return zone
}

func NewBindDnsInstance(serverName string) *BindDnsInstance {
	return &BindDnsInstance{
		Templates:  template.Must(template.ParseGlob("./*.tmpl")),
		ServerName: serverName,
	}
}

// List function
func (di *BindDnsInstance) GetAllZones(zones *ZoneTracker) ([]Zone, *backendError) {
	answer := make([]Zone, 0, 10)
	for k, v := range zones.Zones {
		answer = append(answer, buildZone(k, v))
	}

	return answer, nil
}

// Get function
func (di *BindDnsInstance) GetZone(zones *ZoneTracker, id string) (Zone, *backendError) {
	zdata := zones.Zones[id]
	if zdata == nil {
		return Zone{}, &backendError{"Not Found", 404}
	}

	return buildZone(id, zdata), nil
}

type bindZoneData struct {
	Domain           string
	ServerName       string
	Data             *ZoneData
	ReverseZoneNames []string
}

type bindRZoneData struct {
	Domain     string
	ServerName string
	Name       string
}

func makeRevName(t string, address string) string {
	var buffer bytes.Buffer
	var count, end int

	if t == "A" {
		count = 15
		end = 12
	} else {
		count = 15
		end = 0
	}

	ip := net.ParseIP(address)
	for i := count; i >= end; i-- {
		if t == "A" {
			buffer.WriteString(fmt.Sprintf("%d.", ip[i]))
		} else {
			buffer.WriteString(fmt.Sprintf("%x.", ip[i]&0xf))
			buffer.WriteString(fmt.Sprintf("%x.", ((ip[i] >> 4) & 0xf)))
		}
	}

	if t == "A" {
		buffer.WriteString("in-addr.arpa")
	} else {
		buffer.WriteString("ip6.arpa")
	}

	return buffer.String()
}

// Patch function
func (di *BindDnsInstance) PatchZone(zones *ZoneTracker, zoneName string, rec Record) (Zone, *backendError) {

	// Rebuild include list
	file, err := os.Create("/etc/bind/named.conf.crowbar")
	defer file.Close()
	if err != nil {
		return Zone{}, &backendError{err.Error(), http.StatusInternalServerError}
	}
	err = di.Templates.ExecuteTemplate(file, "zones_list.tmpl", zones)
	if err != nil {
		return Zone{}, &backendError{err.Error(), http.StatusInternalServerError}
	}

	err = os.RemoveAll("/etc/bind/" + zoneName)
	if err != nil {
		return Zone{}, &backendError{err.Error(), http.StatusInternalServerError}
	}

	if zones.Zones[zoneName] != nil {
		revz := make([]string, 0, 100)
		zone := zones.Zones[zoneName]

		err = os.Mkdir("/etc/bind/"+zoneName, 0755)
		if err != nil {
			return Zone{}, &backendError{err.Error(), http.StatusInternalServerError}
		}

		// Build reverse maps
		for name, entry := range zone.Entries {
			for t, contents := range entry.Types {
				for _, content := range contents {
					rvName := makeRevName(t, content.Content)
					revz = append(revz, rvName)

					rdata := bindRZoneData{
						Domain:     rvName,
						ServerName: di.ServerName,
						Name:       name + "." + zoneName,
					}

					rfile, err := os.Create("/etc/bind/" + zoneName + "/rdb." + rvName)
					defer rfile.Close()
					if err != nil {
						return Zone{}, &backendError{err.Error(), http.StatusInternalServerError}
					}
					err = di.Templates.ExecuteTemplate(rfile, "rdb.tmpl", rdata)
					if err != nil {
						return Zone{}, &backendError{err.Error(), http.StatusInternalServerError}
					}

				}
			}
		}

		// Make a database file.
		zdata := bindZoneData{
			Domain:           zoneName,
			ServerName:       di.ServerName,
			Data:             zone,
			ReverseZoneNames: revz,
		}

		zfile, err := os.Create("/etc/bind/" + zoneName + "/zone." + zoneName)
		defer zfile.Close()
		if err != nil {
			return Zone{}, &backendError{err.Error(), http.StatusInternalServerError}
		}
		err = di.Templates.ExecuteTemplate(zfile, "zone.tmpl", zdata)
		if err != nil {
			return Zone{}, &backendError{err.Error(), http.StatusInternalServerError}
		}

		dfile, err := os.Create("/etc/bind/" + zoneName + "/db." + zoneName)
		defer dfile.Close()
		if err != nil {
			return Zone{}, &backendError{err.Error(), http.StatusInternalServerError}
		}
		err = di.Templates.ExecuteTemplate(dfile, "db.tmpl", zdata)
		if err != nil {
			return Zone{}, &backendError{err.Error(), http.StatusInternalServerError}
		}
	}

	// Restart bind
	cmd := exec.Command("service", "named", "restart")
	err = cmd.Run()
	if err != nil {
		return Zone{}, &backendError{err.Error(), http.StatusInternalServerError}
	}

	return buildZone(zoneName, zones.Zones[zoneName]), nil
}
