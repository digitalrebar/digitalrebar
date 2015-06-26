package crowbar

import (
	"log"
	"strconv"
)

// Attrib encapsualtes the concept of per-thing user settable
// parameters that can be overridden in a generic fashion. The way it
// works is like this:
//
// Each thing that can hold Attribs (which in this API is anything
// that implements the Attriber interface) has 1 to 4 buckets that
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
// Additionally, each Attrib has a default value, which is used if
// there is no Attrib value from any of the other buckets.
//
// Not all Attribers have all of these buckets.
type Attrib struct {
	// ID is the database ID number of this attrib.  It has no
	// significance other than uniqueness.
	ID          int64       `json:"id,omitempty"`
	// Name is the human-readable name of the Attrib.  It must be
	// globally unique.
	Name        string      `json:"name,omitempty"`
	// Description is a brief description of what the Attrip is
	// for.
	Description string      `json:"description,omitempty"`
	// BarclampID is the ID of the barclamp that the Attrib was
	// declared in.
	BarclampID  int64       `json:"barclamp_id,omitempty"`
	// RoleID is the ID of the role that this Attrib belongs to.
	// If RoleID is 0, then the attrib does not belong to a role.
	RoleID      int64       `json:"role_id,omitempty"`
	// The custom type of the Attrib, if any.  This is used by
	// Crowbar internally to allow for Attribs to have nonstandard
	// get and set semantics.
	Type        string      `json:"type,omitempty"`
	// Whether the Attrib can be written to.  An attrib must have
	// a non-empty Schema as well as a set Writable flag for a
	// SetAttrib to work, and the Value being passed must validate
	// against the Schema.
	Writable    bool        `json:"writable,omitempty"`
	// Schema is a kwalify schema fragment that the Value must
	// match.  A SetAttrib call with an attrib Value that does not
	// pass schema validation will fail.
	Schema      interface{} `json:"schema,omitempty"`
	// The Map indicates where in the bucket this Attrib should be
	// stored.
	Map         string      `json:"map,omitempty"`
	Order       int64       `json:"order,omitempty"`
	// Value is the value of the Attrib from a specific Attriber.
	Value       interface{} `json:"value,omitempty"`
	// Default is the default value of the Attrib when the
	// Attriber does not otherise have a value.
	Default     interface{} `json:"default,omitempty`
	CreatedAt   string      `json:"created_at,omitempty"`
	UpdatedAt   string      `json:"updated_at,omitempty"`
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

// ApiName returns the pathname that should be used for all API
// operations.
func (o *Attrib) ApiName() string {
	return "attribs"
}

// Attribs gets all the Attribs from a location in the API.  If paths
// is empty, then all Attribs will be fetched, otherwise the paths
// will be joined with "attribs" and we will attempt to fetch the
// Attribs from there.  This behaviour exists because the REST API
// allows you to perform scoped fetches.
func Attribs(paths ...string) (res []*Attrib, err error) {
	res = make([]*Attrib, 0)
	return res, session.list(&res,append(paths, "attribs")...)
}
