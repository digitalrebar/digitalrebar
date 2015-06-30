package crowbar

// Apache 2 License 2015 by Rob Hirschfeld for RackN

import (
	"fmt"
	"log"
	"path"
)

// Crudder defines what is needed to implement basic CRUD operations on an object.
type Crudder interface {
	// Id is a string that can be used to uniquely refer to an object in the REST
	// API.  For most objects this can be either their ID or Name field.
	Id() string
	// ApiName returns the path component in the REST API that refers to this class of object.
	// It will usually be the type name in snake_case.
	ApiName() string
}

// Calculate the API endpoint that should be used to reference this object.
func url(o Crudder, parts ...string) string {
	return path.Join(append([]string{o.ApiName(), o.Id()}, parts...)...)
}

// Read fetches the object from the server.
func Read(o Crudder) error {
	return session.get(o, url(o))
}

// Create creates an object on the server.
func Create(o Crudder) error {
	return session.post(o, o.ApiName())
}

// Destroy removes this object from the server.
func Destroy(o Crudder) error {
	return session.destroy(url(o))
}

// Update updates the server with the current values of this object.
func Update(o Crudder) error {
	return session.put(o, url(o))
}

// Attriber defines what is needed to get and set attribs on an object.
type Attriber interface {
	// You must be a Crudder to be an Attriber.
	Crudder
	// Attribs gets a list of Attribs that pertain to this object.
	Attribs() ([]*Attrib, error)
}

// GetAttrib gets an attrib in the context of an Attriber.  The
// returned Attrib will have its value populated from the contents of
// the passed bucket.  Valid buckets are:
//
//    * "proposed"
//    * "committed"
//    * "system"
//    * "wall"
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
	return res, session.get(res, url)
}

// SetAttrib sets the value of an attrib in the context of
// an attriber.
func SetAttrib(o Attriber, a *Attrib) error {
	return session.put(a, url(o, url(a)))
}

// Propose readies an Attriber to accept new values via SetAttrib.
func Propose(o Attriber) error {
	return session.put(o, url(o, "propose"))
}

// Commit makes the values set on the Attriber via SetAttrib visible
// to the rest of the Crowbar infrastructure.
func Commit(o Attriber) error {
	return session.put(o, url(o, "commit"))
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
