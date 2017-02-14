package store

import "fmt"

// SimpleStore provides an interface for some very basic key/value storage needs.
// This is a flat key/value store.
type SimpleStore interface {
	// Return a new SimpleStore that is subordinate to this one.
	// What exactly that means depends on the simplestore in question.
	Sub(string) (SimpleStore, error)
	// Return the list of keys that this store has
	Keys() ([]string, error)
	// Load the data for a particular key
	Load(string) ([]byte, error)
	// Save data to a key
	Save(string, []byte) error
	// Remove a key/value pair.
	Remove(string) error
}

// NotFound is the "key not found" error type.
type NotFound string

func (n NotFound) Error() string {
	return fmt.Sprintf("key %s: not found", string(n))
}
