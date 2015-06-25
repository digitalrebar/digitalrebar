package crowbar

// Apache 2 License 2015 by Rob Hirschfeld for RackN

import (
	"fmt"
	"path"
	"log"
)

// Crudder implements basic CRUD operations.
type Crudder interface {
	Id() string
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

// Anything that can get Attribs implements Attriber.
type Attriber interface {
	Crudder
	Attribs() ([]*Attrib, error)
}

// GetAttrib gets an attrib in the context of an Attriber.
// The returned Attrib will have its value populated.
func GetAttrib(o Attriber, a *Attrib) (res *Attrib, err error) {
	res = &Attrib{}
	if a.ID != 0 {
		res.ID = a.ID
	} else if a.Name != "" {
		res.Name = a.Name
	} else {
		log.Panicf("Passed Attrib %v does not have a Name or an ID!", a)
	}
	return res, session.get(res, url(o, url(res)))
}

// SetAttrib sets the value of an attrib in the context of
// an attriber.
func SetAttrib(o Attriber, a *Attrib) error {
	return session.put(a, url(o, url(a)))
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
