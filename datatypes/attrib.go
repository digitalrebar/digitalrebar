package datatypes

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
	NameID
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
	Default interface{} `json:"default"`
}

// ApiName returns the pathname that should be used for all API
// operations.
func (o *Attrib) ApiName() string {
	return "attribs"
}
