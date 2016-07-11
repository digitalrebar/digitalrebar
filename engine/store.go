package engine

import (
	"fmt"
	"io/ioutil"
	"os"

	"github.com/hashicorp/consul/api"
)

// LoadSaver is what an Engine will use to save and restore its
// map of global RuleSets.  This interface is liable to change
// as the complexity asd size of the RuleSets grows.
type LoadSaver interface {
	Save([]byte) error
	Load() ([]byte, error)
}

// MemoryStore is a simple LoadSaver that operates
// entirely in memory.  It is used for test purposes.
type MemoryStore []byte

func NewMemoryStore() MemoryStore {
	return MemoryStore([]byte("{}"))
}

func (m MemoryStore) Save(buf []byte) error {
	m = make([]byte, 0, len(buf))
	m = append(m, buf...)
	return nil
}

func (m MemoryStore) Load() []byte {
	res := make([]byte, 0, len(m))
	res = append(res, m...)
	return res
}

// FileStore implements a simple file-based DataStore.
type FileStore struct {
	backingDatabase string
}

func NewFileStore(dbFile string) (*FileStore, error) {
	info, err := os.Stat(dbFile)
	if os.IsNotExist(err) {
		err = ioutil.WriteFile(dbFile, []byte("{}"), 0600)
	}
	info, err = os.Stat(dbFile)
	if err != nil {
		return nil, err
	}
	if !info.Mode().IsRegular() {
		return nil, fmt.Errorf("%s is not a regular file", dbFile)
	}
	return &FileStore{backingDatabase: dbFile}, nil
}

func (fs *FileStore) Save(buf []byte) error {
	return ioutil.WriteFile(fs.backingDatabase, buf, 0600)
}

func (fs *FileStore) Load() ([]byte, error) {
	return ioutil.ReadFile(fs.backingDatabase)
}

// ConsulStore implements a BackingStore using a key in
// the Consul k/v space.
type ConsulStore struct {
	store      *api.KV
	backingKey string
}

func NewConsulStore(key string) (*ConsulStore, error) {
	client, err := api.NewClient(api.DefaultConfig())
	if err != nil {
		return nil, err
	}
	if _, err := client.Agent().Self(); err != nil {
		return nil, err
	}

	store := client.KV()
	pair, _, err := store.Get(key, nil)
	if err != nil {
		return nil, err
	}
	if pair == nil {
		_, err := store.Put(&api.KVPair{Key: key, Value: []byte("{}")}, nil)
		if err != nil {
			return nil, err
		}
	}
	return &ConsulStore{store: store, backingKey: key}, nil
}

func (cs *ConsulStore) Load() ([]byte, error) {
	pair, _, err := cs.store.Get(cs.backingKey, nil)
	if err != nil {
		return []byte{}, err
	}
	return pair.Value, err
}

func (cs *ConsulStore) Save(buf []byte) error {
	_, err := cs.store.Put(&api.KVPair{Key: cs.backingKey, Value: buf}, nil)
	return err
}
