package client

// Apache 2 License 2015 by Rob Hirschfeld for RackN

import (
	"encoding/json"
	"errors"
	"fmt"
	"log"
	"path"
	"reflect"
	"time"

	"github.com/VictorLowther/jsonpatch"
	"github.com/VictorLowther/jsonpatch/utils"
)

// Timestamps records the times an object was created at and last
// updated at on the server.
type Timestamps struct {
	CreatedAt *time.Time `json:"created_at"`
	UpdatedAt *time.Time `json:"updated_at"`
}

// apiHelper tracks some auxillary data used to help out the client API
type apiHelper struct {
	lastJson []byte
	fetchUrl string
}

func (o *apiHelper) setLastJSON(b []byte) {
	o.lastJson = make([]byte, len(b))
	copy(o.lastJson, b)
}

func (o *apiHelper) lastJSON() []byte {
	return o.lastJson
}

func (o *apiHelper) setLastFetch(s string) {
	o.fetchUrl = s
}

func (o *apiHelper) lastFetch() string {
	return o.fetchUrl
}

// Crudder is the interface that a type must satisfy for the client
// API to be able to manage it.
type Crudder interface {
	Id() (string, error)
	ApiName() string
	SetId(string) error
	setLastJSON([]byte)
	lastJSON() []byte
	setLastFetch(string)
	lastFetch() string
}

// Calculate the API endpoint that should be used to reference this object.
func urlFor(o Crudder, parts ...string) string {
	id, err := o.Id()
	if err != nil {
		log.Panic(err)
	}
	return path.Join(append([]string{o.ApiName(), id}, parts...)...)
}

func unmarshal(path string, buf []byte, o Crudder) error {
	err := json.Unmarshal(buf, &o)
	if err != nil {
		return err
	}
	lastbuf, _ := json.Marshal(o)
	o.setLastJSON(lastbuf)
	o.setLastFetch(path)
	return nil
}

// SampleJSON returns the template that should be used to create a new
// object.  This template will have its fields set to the database
// defaults.  It returns the path used to fetch the sample JSON, the
// JSON itself, and an error.
func SampleJSON(o Crudder) (string, []byte, error) {
	uri := path.Join(o.ApiName(), "sample")
	buf, err := session.request("GET", uri, nil)
	return uri, buf, err
}

// Init populates a Crudder with its default values as returned from
// the server.  You should always call Init() on any new object you
// intend to call Create() on in the future to ensure that the
// defaults for the fields are populated correctly.
func Init(o Crudder) error {
	uri, buf, err := SampleJSON(o)
	if err != nil {
		return err
	}
	return unmarshal(uri, buf, o)
}

// Read fetches the object from the server.  The ID must have been
// populated with a previous SetId() call.
func Read(o Crudder) error {
	uri := urlFor(o)
	buf, err := session.request("GET", uri, nil)
	if err != nil {
		return err
	}
	return unmarshal(uri, buf, o)
}

// Fetch combines SetID() and Read().
func Fetch(o Crudder, id string) error {
	if err := SetId(o, id); err != nil {
		return err
	}
	return Read(o)
}

// BaseCreate creates an object on the server. It does NOT prepoulate
// otherwise unused fields with their default vaules.
func BaseCreate(o Crudder) error {
	inbuf, err := json.Marshal(o)
	if err != nil {
		return err
	}
	uri := o.ApiName()
	outbuf, err := session.request("POST", uri, inbuf)
	if err != nil {
		return err
	}
	return unmarshal(uri, outbuf, o)
}

// Create creates a new object on the server.  The new object will consist
// of the results of merging Init(o) with toMerge.  This will ensure that
// all of the otherwise unused fields on the object will be set to their defaults.
func Create(o Crudder, toMerge interface{}) error {
	if err := Init(o); err != nil {
		return err
	}
	var initBuf interface{}
	if err := utils.Remarshal(o, &initBuf); err != nil {
		return err
	}
	merged := utils.Merge(initBuf, toMerge)
	if err := utils.Remarshal(merged, o); err != nil {
		return err
	}
	return BaseCreate(o)
}

// Import also creates a new object on the server, but it passes the JSON in inBuf through unchanged.
// It is intended to be used for object types where we have special-case handling on the server side
// for parameters that are not part of the basic object definition.
func Import(o Crudder, inBuf []byte) error {
	uri := o.ApiName()
	buf, err := session.request("POST", uri, inBuf)
	if err != nil {
		return err
	}
	return unmarshal(uri, buf, o)
}

func safeMergeJSON(target, toMerge []byte) ([]byte, error) {
	targetObj := make(map[string]interface{})
	toMergeObj := make(map[string]interface{})
	if err := json.Unmarshal(target, &targetObj); err != nil {
		return nil, err
	}
	if err := json.Unmarshal(toMerge, &toMergeObj); err != nil {
		return nil, err
	}
	outObj, ok := utils.Merge(targetObj, toMergeObj).(map[string]interface{})
	if !ok {
		return nil, errors.New("Cannot happen in safeMergeJSON")
	}
	keys := make([]string, 0)
	for k := range outObj {
		if _, ok := targetObj[k]; !ok {
			keys = append(keys, k)
		}
	}
	for _, k := range keys {
		delete(outObj, k)
	}
	return json.Marshal(outObj)
}

// Create creates a new object on the server, merging in the
// JSON that toMerge contains to initially populate the fields.
// The passed-in Crudder will be overwritten with the new object.
func CreateJSON(o Crudder, toMerge []byte) error {
	var buf interface{}
	if err := json.Unmarshal(toMerge, &buf); err != nil {
		return err
	}
	return Create(o, buf)
}

// Destroy removes this object from the server.
func Destroy(o Crudder) error {
	_, err := session.request("DELETE", urlFor(o), nil)
	return err
}

// Patch attempts to update the object with patch, which must be an RFC6902 JSON Patch.
func Patch(o Crudder, patch []byte) error {
	uri := urlFor(o)
	outbuf, err := session.request("PATCH", uri, patch)
	if err != nil {
		return err
	}
	return unmarshal(uri, outbuf, o)
}

// MakePatch generates a JSON Patch that describes the difference between the last time
// an object was fetched from the server and its current state.
func MakePatch(o Crudder) ([]byte, error) {
	if len(o.lastJSON()) == 0 {
		return nil, fmt.Errorf("Cannot update an object that has never been fetched")
	}
	buf, err := json.Marshal(o)
	if err != nil {
		return nil, err
	}
	return jsonpatch.GenerateJSON(o.lastJSON(), buf, true)
}

// Update updates the server with the current values of this object.
// Update uses RFC 6092 JSON patches generated with test before remove
// or replace stanzas to ensure that the server has all the
// information it needs to safely update the object or reject updates
// if things have changed.
func Update(o Crudder) error {
	patch, err := MakePatch(o)
	if err != nil {
		return err
	}
	return Patch(o, patch)
}

// UpdateJSON tries to update Crudder with toMerge, which should be a
// JSON blob with keys that correspond to the Crudder's struct fields
// when serialzed to JSON.
func UpdateJSON(o Crudder, toMerge []byte) error {
	buf := o.lastJSON()
	if len(buf) == 0 {
		return fmt.Errorf("Cannot update an object that has never been fetched")
	}
	merged, err := safeMergeJSON(buf, toMerge)
	if err != nil {
		return err
	}
	patch, err := jsonpatch.GenerateJSON(buf, merged, true)
	if err != nil {
		return err
	}
	return Patch(o, patch)
}

func updatePaths(p string, val interface{}) {
	v := reflect.ValueOf(val)
	if v.Kind() != reflect.Slice {
		return
	}
	for i := 0; i < v.Len(); i++ {
		o, ok := v.Index(i).Interface().(Crudder)
		if !ok {
			return
		}

		id, err := o.Id()
		if err != nil {
			log.Panic(err)
		}
		lastbuf, _ := json.Marshal(o)
		o.setLastJSON(lastbuf)
		o.setLastFetch(path.Join(p, id))
	}
}

// Match selects the objects from path (which should be generated by
// ApiPath() matchiing the key/value pairs in vals (the keys of which
// should be named the same as the JSON firld names you want to match
// against), and res is where to put the results.
func Match(path string, vals map[string]interface{}, res interface{}) error {
	err := session.match(vals, res, path)
	updatePaths(path, res)
	return err

}

// List fetches all the object available from path, and unmarshals
// them into res.
func List(path string, res interface{}) error {
	err := session.list(res, path)
	updatePaths(path, res)
	return err
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
