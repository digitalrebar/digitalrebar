package store

import "fmt"

// SimpleStore provides an interface for some very basic key/value
// storage needs.  Each SimpleStore (including ones created with Sub()
// should operate as seperate, flat key/value stores.
type SimpleStore interface {
	// Return a new SimpleStore that is subordinate to this one.
	// What exactly that means depends on the simplestore in question,
	// but it should wind up sharing the same backing store (directory, database, etcd cluster, whatever)
	Sub(string) (SimpleStore, error)
	// Return the list of keys that this store has in no particular order.
	Keys() ([]string, error)
	// Return all the data in the store in no particular order.
	List() ([][]byte, error)
	// Load the data for a particular key
	Load(string) ([]byte, error)
	// Save data to a key
	Save(string, []byte) error
	// Remove a key/value pair.
	Remove(string) error
}

func genericList(s SimpleStore) ([][]byte, error) {
	keys, err := s.Keys()
	if err != nil {
		return nil, err
	}
	res := make([][]byte, len(keys))
	for i := range keys {
		val, err := s.Load(keys[i])
		if err != nil {
			return nil, err
		}
		res[i] = val
	}
	return res, nil
}

// NotFound is the "key not found" error type.
type NotFound string

func (n NotFound) Error() string {
	return fmt.Sprintf("key %s: not found", string(n))
}
