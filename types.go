package crowbar

// Apache 2 License 2015 by Rob Hirschfeld for RackN

import (
	"encoding/json"
	"fmt"
	"log"
	"path"
	"reflect"
	"time"

	"github.com/VictorLowther/jsonpatch"
	"github.com/VictorLowther/jsonpatch/utils"
)

type Timestamps struct {
	CreatedAt *time.Time `json:"created_at"`
	UpdatedAt *time.Time `json:"updated_at"`
}

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
func url(o Crudder, parts ...string) string {
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

// Init populates a Crudder with its default values as returned from the server.
// You should always call Init() on any new object you intend to call Create() on in the
// future to ensure that the defaults for the fields are populated correctly.
func Init(o Crudder) error {
	uri, buf, err := SampleJSON(o)
	if err != nil {
		return err
	}
	return unmarshal(uri, buf, o)
}

// Read fetches the object from the server.
func Read(o Crudder) error {
	uri := url(o)
	buf, err := session.request("GET", uri, nil)
	if err != nil {
		return err
	}
	return unmarshal(uri, buf, o)
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

// Create creates a new object on the server, merging in the
// JSON that toMerge contains to initially populate the fields.
// The passed-in Crudder will be overwritten with the new object.
func CreateJSON(o Crudder, toMerge []byte) error {
	_, buf, err := SampleJSON(o)
	if err != nil {
		return err
	}
	merged, err := utils.MergeJSON(buf, toMerge)
	if err != nil {
		return err
	}
	if err := json.Unmarshal(merged, &o); err != nil {
		return err
	}
	return BaseCreate(o)
}

// Destroy removes this object from the server.
func Destroy(o Crudder) error {
	_, err := session.request("DELETE", url(o), nil)
	return err
}

// Patch attempts to update o with patch, which must be an RFC6902 JSON Patch.
func Patch(o Crudder, patch []byte) error {
	uri := url(o)
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
	merged, err := utils.MergeJSON(buf, toMerge)
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
