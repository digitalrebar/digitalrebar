package crowbar

// Apache 2 License 2015 by Rob Hirschfeld for RackN

import (
	"fmt"
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
	// SetId sets the ID of an object.
	SetId(string) error
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
