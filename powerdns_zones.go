package main

/* These are needed for database access
 *   "database/sql"
 *    _ "github.com/lib/pq"
 *
 * These are needed for consul api accces
 *    "github.com/hashicorp/consul/api"
 */

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"net/http"
)

/*
 * PowerDNS API Structures
 *
 * These match the json objects that are needed to
 * update/create and get zone information and records
 */
type PowerDnsZone struct {
	Id               string            `json:"id"`
	Name             string            `json:"name"`
	Url              string            `json:"url"`
	Kind             string            `json:"kind"`
	Type             string            `json:"type,omitempty"`
	Dnssec           bool              `json:"dnssec"`
	Account          string            `json:"account"`
	Serial           int               `json:"serial"`
	NotifiedSerial   int               `json:"notified_serial"`
	Servers          []string          `json:"servers,omitempty"`
	Masters          []string          `json:"masters,omitempty"`
	Nameservers      []string          `json:"nameservers,omitempty"`
	RecursionDesired bool              `json:"recursion_desired,omitempty"`
	LastCheck        int               `json:"last_check"`
	SoaEdit          string            `json:"soa_edit,omitempty"`
	SoaEditApi       string            `json:"soa_edit_api,omitempty"`
	PowerDnsComments []PowerDnsComment `json:"comments,omitempty"`
	PowerDnsRecords  []PowerDnsRecord  `json:"records,omitempty"`
}

type PowerDnsRRSets struct {
	PowerDnsRRSets []PowerDnsRRSet `json:"rrsets"`
}

type PowerDnsRRSet struct {
	Name             string            `json:"name"`
	Type             string            `json:"type"`
	ChangeType       string            `json:"changetype"`
	PowerDnsRecords  []PowerDnsRecord  `json:"records,omitempty"`
	PowerDnsComments []PowerDnsComment `json:"comments,omitempty"`
}

type PowerDnsComment struct {
	Content    string `json:"content"`
	Account    string `json:"account"`
	ModifiedAt int    `json:"modified_at"`
	Type       string `json:"type,omitempty"`
	Name       string `json:"name,omitempty"`
}

type PowerDnsRecord struct {
	Content  string `json:"content"`
	Name     string `json:"name"`
	TTL      int    `json:"ttl"`
	Type     string `json:"type"`
	Disabled bool   `json:"disabled"`
	SetPtr   bool   `json:"set-ptr,omitempty"`
	Priority int    `json:"priority"`
}

type PowerDnsError struct {
	Error string `json:"error"`
}

type PowerDnsInstance struct {
	UrlBase     string
	AccessToken string
	dns_backend_point
}

func (di *PowerDnsInstance) makeZoneUrl(id *string) string {
	if id != nil {
		return fmt.Sprintf("%s/%s/%s", di.UrlBase, "zones", *id)
	}
	return fmt.Sprintf("%s/%s", di.UrlBase, "zones")
}

func (di *PowerDnsInstance) doURL(action, url string, data io.Reader) ([]byte, *backendError) {
	req, err := http.NewRequest(action, url, data)
	if err != nil {
		return nil, &backendError{err.Error(), 500}
	}
	req.Header.Set("X-API-Key", di.AccessToken)
	req.Header.Set("Content-Type", "application/json")

	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		return nil, &backendError{err.Error(), 500}
	}
	defer resp.Body.Close()

	body, _ := ioutil.ReadAll(resp.Body)

	// If they return an error, we should as well.
	if resp.StatusCode > 399 {
		var data PowerDnsError
		jerr := json.Unmarshal(body, &data)
		if jerr != nil {
			return body, &backendError{string(body), resp.StatusCode}
		}

		return body, &backendError{data.Error, resp.StatusCode}
	}

	return body, nil
}

// List function
func (di *PowerDnsInstance) GetAllZones(zones *ZoneTracker) ([]Zone, *backendError) {
	url := di.makeZoneUrl(nil)
	body, err := di.doURL("GET", url, nil)
	if err != nil {
		return nil, err
	}

	var pzs []PowerDnsZone
	jerr := json.Unmarshal(body, &pzs)
	if jerr != nil {
		log.Panic(jerr)
	}

	return marshalPowerDnsZonesToZones(pzs), nil
}

// Get function
func (di *PowerDnsInstance) GetZone(zones *ZoneTracker, id string) (Zone, *backendError) {
	url := di.makeZoneUrl(&id)
	body, err := di.doURL("GET", url, nil)
	if err != nil {
		return Zone{}, err
	}

	var pdz PowerDnsZone
	jerr := json.Unmarshal(body, &pdz)
	if jerr != nil {
		log.Panic(jerr)
	}

	data := marshalPowerDnsZoneToZone(pdz)

	return data, nil
}

// Patch function
func (di *PowerDnsInstance) PatchZone(zones *ZoneTracker, zoneName string, rec Record) (Zone, *backendError) {

	// GREG: Build replace/remove call

	/* Like this:
			data = {
				'rrsets' => [
						{
								'name' => name,
								'type' => rr_type,
								'changetype' => 'REPLACE',
								'records' => [
										{
												'content' => value,
												'disabled' => false,
												'name' => name,
												'ttl' => 3600,
												'type' => rr_type,
												'setptr' => setptr,
												'priority' => 0
										}
								]
						}
				]
	  	}

			data = {
					'rrsets' => [
							{
									'name' => name,
									'type' => rr_type,
									'changetype' => 'DELETE',
									'records' => [ ]
							}
					]
			}
	*/

	url := di.makeZoneUrl(&zoneName)
	b, berr := json.Marshal(zones.Zones[zoneName])
	if berr != nil {
		log.Panic(berr)
	}
	body, derr := di.doURL("PATCH", url, bytes.NewReader(b))
	if derr != nil {
		return Zone{}, derr
	}

	var pdz PowerDnsZone
	err := json.Unmarshal(body, &pdz)
	if err != nil {
		log.Panic(err)
	}

	data := marshalPowerDnsZoneToZone(pdz)

	return data, nil
}

func marshalPowerDnsZonesToZones(pz []PowerDnsZone) []Zone {
	z := []Zone{}
	//GREG: Fix this
	return z
}

func marshalPowerDnsZoneToZone(pz PowerDnsZone) Zone {
	z := Zone{}
	//GREG: Fix this
	return z
}
