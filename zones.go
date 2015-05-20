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
	"github.com/ant0ine/go-json-rest/rest"
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
type Zone struct {
	Id               string    `json:"id"`
	Name             string    `json:"name"`
	Url              string    `json:"url"`
	Kind             string    `json:"kind"`
	Type             string    `json:"type,omitempty"`
	Dnssec           bool      `json:"dnssec"`
	Account          string    `json:"account"`
	Serial           int       `json:"serial"`
	NotifiedSerial   int       `json:"notified_serial"`
	Servers          []string  `json:"servers,omitempty"`
	Masters          []string  `json:"masters,omitempty"`
	Nameservers      []string  `json:"nameservers,omitempty"`
	RecursionDesired bool      `json:"recursion_desired,omitempty"`
	LastCheck        int       `json:"last_check"`
	SoaEdit          string    `json:"soa_edit,omitempty"`
	SoaEditApi       string    `json:"soa_edit_api,omitempty"`
	Comments         []Comment `json:"comments,omitempty"`
	Records          []Record  `json:"records,omitempty"`
}

type RRSets struct {
	RRSets []RRSet `json:"rrsets"`
}

type RRSet struct {
	Name       string    `json:"name"`
	Type       string    `json:"type"`
	ChangeType string    `json:"changetype"`
	Records    []Record  `json:"records,omitempty"`
	Comments   []Comment `json:"comments,omitempty"`
}

type Comment struct {
	Content    string `json:"content"`
	Account    string `json:"account"`
	ModifiedAt int    `json:"modified_at"`
	Type       string `json:"type,omitempty"`
	Name       string `json:"name,omitempty"`
}

type Record struct {
	Content  string `json:"content"`
	Name     string `json:"name"`
	TTL      int    `json:"ttl"`
	Type     string `json:"type"`
	Disabled bool   `json:"disabled"`
	SetPtr   bool   `json:"set-ptr,omitempty"`
	Priority int    `json:"priority"`
}

type PdnsError struct {
	Error string `json:"error"`
}

func (di *DnsInstance) makeZoneUrl(id *string) string {
	if id != nil {
		return fmt.Sprintf("%s/%s/%s", di.UrlBase, "zones", *id)
	}
	return fmt.Sprintf("%s/%s", di.UrlBase, "zones")
}

type doURLError struct {
	s           string
	status_code int
}

func (e *doURLError) Error() string {
	return e.s
}

func (e *doURLError) StatusCode() int {
	return e.status_code
}

func (di *DnsInstance) doURL(action, url string, data io.Reader) ([]byte, *doURLError) {
	req, err := http.NewRequest(action, url, data)
	if err != nil {
		return nil, &doURLError{err.Error(), 500}
	}
	req.Header.Set("X-API-Key", di.AccessToken)
	req.Header.Set("Content-Type", "application/json")

	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		return nil, &doURLError{err.Error(), 500}
	}
	defer resp.Body.Close()

	body, _ := ioutil.ReadAll(resp.Body)

	// If they return an error, we should as well.
	if resp.StatusCode > 399 {
		var data PdnsError
		jerr := json.Unmarshal(body, &data)
		if jerr != nil {
			return body, &doURLError{string(body), resp.StatusCode}
		}

		return body, &doURLError{data.Error, resp.StatusCode}
	}

	return body, nil
}

// List function
func (di *DnsInstance) GetAllZones(w rest.ResponseWriter, r *rest.Request) {
	url := di.makeZoneUrl(nil)
	body, err := di.doURL("GET", url, nil)
	if err != nil {
		rest.Error(w, err.Error(), err.StatusCode())
		return
	}

	var data []Zone
	jerr := json.Unmarshal(body, &data)
	if jerr != nil {
		log.Panic(jerr)
	}

	w.WriteJson(data)
}

// Get function
func (di *DnsInstance) GetZone(w rest.ResponseWriter, r *rest.Request) {
	id := r.PathParam("id")
	url := di.makeZoneUrl(&id)
	body, err := di.doURL("GET", url, nil)
	if err != nil {
		rest.Error(w, err.Error(), err.StatusCode())
		return
	}

	var data Zone
	jerr := json.Unmarshal(body, &data)
	if jerr != nil {
		log.Panic(jerr)
	}

	w.WriteJson(data)
}

// Create function
func (di *DnsInstance) PostZone(w rest.ResponseWriter, r *rest.Request) {
	zone := Zone{}
	err := r.DecodeJsonPayload(&zone)
	if err != nil {
		rest.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	url := di.makeZoneUrl(nil)
	b, berr := json.Marshal(zone)
	if berr != nil {
		log.Panic(berr)
	}
	body, derr := di.doURL("POST", url, bytes.NewReader(b))
	if derr != nil {
		rest.Error(w, derr.Error(), derr.StatusCode())
		return
	}

	var data Zone
	err = json.Unmarshal(body, &data)
	if err != nil {
		log.Panic(err)
	}

	w.WriteJson(data)
}

// Update function
func (di *DnsInstance) PutZone(w rest.ResponseWriter, r *rest.Request) {
	zone := Zone{}
	err := r.DecodeJsonPayload(&zone)
	if err != nil {
		rest.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	id := r.PathParam("id")
	url := di.makeZoneUrl(&id)
	b, berr := json.Marshal(zone)
	if berr != nil {
		log.Panic(berr)
	}
	body, derr := di.doURL("PUT", url, bytes.NewReader(b))
	if derr != nil {
		rest.Error(w, derr.Error(), derr.StatusCode())
		return
	}

	var data Zone
	err = json.Unmarshal(body, &data)
	if err != nil {
		log.Panic(err)
	}

	w.WriteJson(data)
}

// Delete function
func (di *DnsInstance) DeleteZone(w rest.ResponseWriter, r *rest.Request) {
	id := r.PathParam("id")
	url := di.makeZoneUrl(&id)
	_, err := di.doURL("DELETE", url, nil)
	if err != nil {
		rest.Error(w, err.Error(), err.StatusCode())
	}
}

// Patch function
func (di *DnsInstance) PatchZone(w rest.ResponseWriter, r *rest.Request) {
	rrsets := RRSets{}
	err := r.DecodeJsonPayload(&rrsets)
	if err != nil {
		rest.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	id := r.PathParam("id")
	url := di.makeZoneUrl(&id)
	b, berr := json.Marshal(rrsets)
	if berr != nil {
		log.Panic(berr)
	}
	body, derr := di.doURL("PATCH", url, bytes.NewReader(b))
	if derr != nil {
		rest.Error(w, derr.Error(), derr.StatusCode())
		return
	}

	var data Zone
	err = json.Unmarshal(body, &data)
	if err != nil {
		log.Panic(err)
	}

	w.WriteJson(data)
}
