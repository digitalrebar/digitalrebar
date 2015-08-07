package crowbar

import (
	"encoding/json"
	"errors"
	"fmt"
	"log"
	"strconv"
)

// Attrib encapsualtes the concept of per-thing user settable
// parameters that can be overridden in a generic fashion. The way it
// works is like this:
//
// Each thing that can hold Attribs (which in this API is anything
// that implements the Attriber interface) has 2 to 5 buckets that
// values can be stored in. Each bucket is implemented on the server
// side as a large JSON blob that holds the Attrib values.  This
// implementation is likely to change, so this API does not expose
// access to the buckets directly.
//
// The first bucket (and the only one that this API can write to) is
// the proposed bucket.  Updating the value of an Attrib via the API
// or the UI writes the value into this bucket unless otherwise
// indicated, and the annealer ignores attrib values from the proposed
// bucket.
//
// The second bucket is the committed bucket. After you have made
// whatever changes you want to the attribs of an Attriber, the
// proposed bucket is copied over to the committed bucket.
//
// The third bucket is the system bucket.  This bucket contains
// information that the Crowbar framework maintains internally for the
// logic that the Roles implement to use.
//
// The fourth bucket is the wall.  It contains the values of attribs
// that are updated as a consequence of a Role being run against a
// Node
//
// The fifth bucket is the notes.  Notes do not participate in the
// annealing process, and are inteded to be used by users for
// attaching arbitrary data that they need to remember but that
// Crowbar does not need to care about.
//
// Additionally, each Attrib has a default value, which is used if
// there is no Attrib value from any of the other buckets.
//
// Not all Attribers have all of these buckets.
type Attrib struct {
	// ID is the database ID number of this attrib.  It has no
	// significance other than uniqueness.
	ID int64 `json:"id"`
	// Name is the human-readable name of the Attrib.  It must be
	// globally unique.
	Name string `json:"name"`
	// Description is a brief description of what the Attrip is
	// for.
	Description string `json:"description"`
	// BarclampID is the ID of the barclamp that the Attrib was
	// declared in.
	BarclampID int64 `json:"barclamp_id"`
	// RoleID is the ID of the role that this Attrib belongs to.
	// If RoleID is 0, then the attrib does not belong to a role.
	RoleID int64 `json:"role_id"`
	// The custom type of the Attrib, if any.  This is used by
	// Crowbar internally to allow for Attribs to have nonstandard
	// get and set semantics.
	Type string `json:"type"`
	// Whether the Attrib can be written to.  An attrib must have
	// a non-empty Schema as well as a set Writable flag for a
	// SetAttrib to work, and the Value being passed must validate
	// against the Schema.
	Writable bool `json:"writable"`
	// Schema is a kwalify schema fragment that the Value must
	// match.  A SetAttrib call with an attrib Value that does not
	// pass schema validation will fail.
	Schema interface{} `json:"schema"`
	// The Map indicates where in the bucket this Attrib should be
	// stored.
	Map   string `json:"map"`
	Order int64  `json:"order"`
	// Value is the value of the Attrib from a specific Attriber.
	Value interface{} `json:"value"`
	// Default is the default value of the Attrib when the
	// Attriber does not otherise have a value.
	Default   interface{} `json:"default"`
	CreatedAt string      `json:"created_at"`
	UpdatedAt string      `json:"updated_at"`
	lastJson  []byte
}

// Id returns this attrib's ID or Name as a string.
// The REST API allows them to be used interchangeably.
func (o *Attrib) Id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	} else if o.Name != "" {
		return o.Name
	} else {
		log.Panic("Attrib has no ID or name")
		return ""
	}
}

func (o *Attrib) SetId(s string) error {
	if o.ID != 0 || o.Name != "" {
		return errors.New("SetId can only be used on an un-IDed object")
	}
	if id, err := strconv.ParseInt(s, 10, 64); err == nil {
		o.ID = id
	} else {
		o.Name = s
	}
	return nil
}

func (o *Attrib) setLastJSON(b []byte) {
	o.lastJson = make([]byte, len(b))
	copy(o.lastJson, b)
}

func (o *Attrib) lastJSON() []byte {
	return o.lastJson
}

// ApiName returns the pathname that should be used for all API
// operations.
func (o *Attrib) ApiName() string {
	return "attribs"
}

func (o *Attrib) Match() (res []*Attrib, err error) {
	res = make([]*Attrib, 0)
	return res, session.match(o, &res, o.ApiName(), "match")
}

// Attriber defines what is needed to get and set attribs on an object.
type Attriber interface {
	// You must be a Crudder to be an Attriber.
	Crudder
	// satisfy the Attriber interface.
	attribs()
}

// Attribs gets all the Attribs from a location in the API.  If paths
// is empty, then all Attribs will be fetched, otherwise the paths
// will be joined with "attribs" and we will attempt to fetch the
// Attribs from there.  This behaviour exists because the REST API
// allows you to perform scoped fetches.
func Attribs(scope ...Attriber) (res []*Attrib, err error) {
	res = make([]*Attrib, 0)
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = url(scope[i])
	}

	return res, session.list(&res, append(paths, "attribs")...)
}

// GetAttrib gets an attrib in the context of an Attriber.  The
// returned Attrib will have its value populated from the contents of
// the passed bucket.  Valid buckets are:
//
//    * "proposed"
//    * "committed"
//    * "system"
//    * "wall"
//    * "note"
//    * "all"
func GetAttrib(o Attriber, a *Attrib, bucket string) (res *Attrib, err error) {
	res = &Attrib{}
	if a.ID != 0 {
		res.ID = a.ID
	} else if a.Name != "" {
		res.Name = a.Name
	} else {
		log.Panicf("Passed Attrib %v does not have a Name or an ID!", a)
	}
	url := url(o, url(res))
	if bucket != "" {
		url = fmt.Sprintf("%v?bucket=%v", url, bucket)
	}
	inbuf, err := json.Marshal(res)
	if err != nil {
		return res, err
	}
	outbuf, err := session.request("GET", url, inbuf)
	if err != nil {
		return res, err
	}
	return res, unmarshal(outbuf, res)
}

// SetAttrib sets the value of an attrib in the context of
// an attriber in the passed bucket.  Valid buckets are:
//
//    * "user"
//    * "note"
func SetAttrib(o Attriber, a *Attrib, bucket string) error {
	if bucket == "" {
		bucket = "user"
	}
	uri := fmt.Sprintf("%v?bucket=%v", url(o, url(a)), bucket)
	inbuf, err := json.Marshal(a)
	if err != nil {
		return err
	}
	outbuf, err := session.request("PUT", uri, inbuf)
	if err != nil {
		return err
	}
	return unmarshal(outbuf, a)
}

// Propose readies an Attriber to accept new values via SetAttrib.
func Propose(o Attriber) error {
	inbuf, err := json.Marshal(o)
	if err != nil {
		return err
	}
	outbuf, err := session.request("PUT", url(o, "propose"), inbuf)
	if err != nil {
		return err
	}
	return unmarshal(outbuf, o)
}

// Commit makes the values set on the Attriber via SetAttrib visible
// to the rest of the Crowbar infrastructure.
func Commit(o Attriber) error {
	inbuf, err := json.Marshal(o)
	if err != nil {
		return err
	}
	outbuf, err := session.request("PUT", url(o, "commit"), inbuf)
	if err != nil {
		return err
	}
	return unmarshal(outbuf, o)
}
