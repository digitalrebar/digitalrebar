package crowbar

// Apache 2 License 2015 by Rob Hirschfeld for RackN

import (
	"encoding/json"
	"fmt"
	"path"

	"github.com/VictorLowther/jsonpatch"
)

// Crudder defines what is needed to implement basic CRUD operations on an object.
type Crudder interface {
	// Id is a string that can be used to uniquely refer to an object in the REST
	// API.  For most objects this can be either their ID or Name field.
	Id() string
	// ApiName returns the path component in the REST API that refers to this class of object.
	// It will usually be the type name in snake_case.
	ApiName() string
	// SetId sets the ID of an object.
	SetId(string) error
	// setLastJSON saves a copy of the last JSON that was used to instantiate the Crudder.
	setLastJSON([]byte)
	// lastJSON returns the byte array of the last JSON that was use to instantiate the object.
	lastJSON() []byte
}

// Calculate the API endpoint that should be used to reference this object.
func url(o Crudder, parts ...string) string {
	return path.Join(append([]string{o.ApiName(), o.Id()}, parts...)...)
}

func unmarshal(buf []byte, o Crudder) error {
	err := json.Unmarshal(buf, &o)
	if err != nil {
		return err
	}
	lastbuf, _ := json.Marshal(o)
	o.setLastJSON(lastbuf)
	return nil
}

// Read fetches the object from the server.
func Read(o Crudder) error {
	buf, err := session.request("GET", url(o), nil)
	if err != nil {
		return err
	}
	return unmarshal(buf, o)
}

// Create creates an object on the server.
func Create(o Crudder) error {
	inbuf, err := json.Marshal(o)
	if err != nil {
		return err
	}
	outbuf, err := session.request("POST", o.ApiName(), inbuf)
	if err != nil {
		return err
	}
	return unmarshal(outbuf, o)
}

// Destroy removes this object from the server.
func Destroy(o Crudder) error {
	_, err := session.request("DELETE", url(o), nil)
	return err
}

// Patch attempts to update o with patch, which must be an RFC6902 JSON Patch.
func Patch(o Crudder, patch []byte) error {
	outbuf, err := session.request("PATCH", url(o), patch)
	if err != nil {
		return err
	}
	return unmarshal(outbuf, o)
}

// Update updates the server with the current values of this object.
// Update uses RFC 6092 JSON patches generated with test before remove
// or replace stanzas to ensure that the server has all the
// information it needs to safely update the object or reject updates
// if things have changed.
func Update(o Crudder) error {
	if len(o.lastJSON()) == 0 {
		return fmt.Errorf("Cannot update an object that has never been fetched")
	}
	target, err := json.Marshal(o)
	if err != nil {
		return err
	}
	patch, err := jsonpatch.GenerateJSON(o.lastJSON(), target, true)
	if err != nil {
		return err
	}
	return Patch(o, patch)
}

func Match(path string, vals map[string]interface{}, res interface{}) error {
	return session.match(vals, res, path)
}

func List(path string, res interface{}) error {
	return session.list(res, path)
}

// SetId sets the ID of an object.
//
// If id can be parsed as an int64 without error, o's ID field will
// be populated with the results of that conversion, otherwise the Name
// field will be populated with passed string.
// An error will be returned if the object already has a set Name or ID field,
// or if the object does not have a Name field and the passed string cannot be
// parsed to an int64
func SetId(o Crudder, id string) error {
	return o.SetId(id)
}

type CrowbarDigest struct {
	CSRFToken string `json:"csrf_token"`
	Message   string `json:"message"`
}

type Error struct {
	Message   string `json:"message"`
	Status    int    `json:"status"`
	Backtrace string `json:"backtrace"`
}

func (e *Error) Error() string {
	return fmt.Sprintf("Status: %v\nMessage: %v\n Backtrace: %v")
}

type NodePower struct {
	ID     int64  `json:"id"`
	Action string `json:"action"`
	Result string `json:"result"`
}

type NodeAddress struct {
	Node      string   `json:"node"`
	Network   string   `json:"network"`
	Category  string   `json:"category"`
	Addresses []string `json:"addresses"`
}
