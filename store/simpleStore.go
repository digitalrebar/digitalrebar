package store

import (
	"fmt"
	"path"
	"path/filepath"
	"sort"
	"sync"

	"github.com/boltdb/bolt"
	consul "github.com/hashicorp/consul/api"
)

// SimpleStore provides an interface for some very basic key/value storage needs.
// This is a flat key/value store.
type SimpleStore interface {
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

// MemoryStore provides an in-memory implementation of SimpleStore
// for testing purposes
type SimpleMemoryStore struct {
	sync.RWMutex
	v map[string][]byte
}

func NewSimpleMemoryStore() *SimpleMemoryStore {
	return &SimpleMemoryStore{v: make(map[string][]byte)}
}

func (m *SimpleMemoryStore) Keys() ([]string, error) {
	m.RLock()
	res := make([]string, 0, len(m.v))
	for k := range m.v {
		res = append(res, k)
	}
	m.RUnlock()
	sort.Strings(res)
	return res, nil
}

func (m *SimpleMemoryStore) Load(key string) ([]byte, error) {
	m.RLock()
	v, ok := m.v[key]
	m.RUnlock()
	if !ok {
		return v, NotFound(key)
	}
	return v, nil
}

func (m *SimpleMemoryStore) Save(key string, val []byte) error {
	m.Lock()
	m.v[key] = val
	m.Unlock()
	return nil
}

func (m *SimpleMemoryStore) Remove(key string) error {
	m.Lock()
	delete(m.v, key)
	m.Unlock()
	return nil
}

type SimpleLocalStore struct {
	db     *bolt.DB
	bucket []byte
}

func NewSimpleLocalStore(location string) (*SimpleLocalStore, error) {
	res := &SimpleLocalStore{bucket: []byte("Default")}
	db, err := bolt.Open(filepath.Clean(filepath.Join(location, "bolt.db")), 0600, nil)
	if err != nil {
		return nil, err
	}
	res.db = db
	return res, res.db.Update(func(tx *bolt.Tx) error {
		_, err := tx.CreateBucketIfNotExists(res.bucket)
		return err
	})
}

func (b *SimpleLocalStore) Keys() ([]string, error) {
	res := []string{}
	err := b.db.View(func(tx *bolt.Tx) error {
		bucket := tx.Bucket(b.bucket)
		bucket.ForEach(func(k, v []byte) error {
			res = append(res, string(k))
			return nil
		})
		return nil
	})
	return res, err
}

func (b *SimpleLocalStore) Load(key string) ([]byte, error) {
	var res []byte
	err := b.db.View(func(tx *bolt.Tx) error {
		bucket := tx.Bucket(b.bucket)
		res = bucket.Get([]byte(key))
		if res == nil {
			return NotFound(key)
		}
		return nil
	})
	return res, err
}

func (b *SimpleLocalStore) Save(key string, val []byte) error {
	return b.db.Update(func(tx *bolt.Tx) error {
		bucket := tx.Bucket(b.bucket)
		return bucket.Put([]byte(key), val)
	})
}

func (b *SimpleLocalStore) Remove(key string) error {
	return b.db.Update(func(tx *bolt.Tx) error {
		bucket := tx.Bucket(b.bucket)
		return bucket.Delete([]byte(key))
	})
}

type SimpleConsulStore struct {
	kv      *consul.KV
	baseKey string
}

func NewSimpleConsulStore(c *consul.Client, prefix string) (*SimpleConsulStore, error) {
	return &SimpleConsulStore{kv: c.KV(), baseKey: prefix}, nil
}

func (b *SimpleConsulStore) finalKey(k string) string {
	return path.Clean(path.Join(b.baseKey, k))
}

func (b *SimpleConsulStore) Keys() ([]string, error) {
	res, _, err := b.kv.Keys(b.baseKey, "", nil)
	return res, err
}

func (b *SimpleConsulStore) Load(key string) ([]byte, error) {
	val, _, err := b.kv.Get(b.finalKey(key), nil)
	return val.Value, err
}

func (b *SimpleConsulStore) Save(key string, val []byte) error {
	kp := &consul.KVPair{Value: val, Key: b.finalKey(key)}
	_, err := b.kv.Put(kp, nil)
	return err
}

func (b *SimpleConsulStore) Remove(key string) error {
	_, err := b.kv.Delete(b.finalKey(key), nil)
	return err
}
