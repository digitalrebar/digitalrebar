package crowbar

import (
	"path"
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
