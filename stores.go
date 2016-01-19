package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"

	"github.com/hashicorp/consul/api"
)

type LoadSaver interface {
	Save(*ZoneTracker) error
	Load(*ZoneTracker) error
}

type FileStore struct {
	backingDatabase string
}

func NewFileStore(dbFile string) (*FileStore, error) {
	info, err := os.Stat(dbFile)
	if err != nil {
		return nil, err
	}
	if !info.Mode().IsRegular() {
		return nil, fmt.Errorf("%s is not a regular file", dbFile)
	}
	return &FileStore{backingDatabase: dbFile}, nil
}

func (fs *FileStore) Save(zt *ZoneTracker) error {
	data, err := json.Marshal(zt)
	if err != nil {
		return err
	}
	if err := ioutil.WriteFile(fs.backingDatabase, data, 0700); err != nil {
		return err
	}
	return nil
}

func (fs *FileStore) Load(zt *ZoneTracker) error {
	zt.Lock()
	defer zt.Unlock()
	data, err := ioutil.ReadFile(fs.backingDatabase)
	if err != nil {
		return err
	}
	return json.Unmarshal(data, &zt)
}

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

func (cs *ConsulStore) Load(zt *ZoneTracker) error {
	zt.Lock()
	defer zt.Unlock()
	pair, _, err := cs.store.Get(cs.backingKey, nil)
	if err != nil {
		return err
	}
	return json.Unmarshal(pair.Value, &zt)
}

func (cs *ConsulStore) Save(zt *ZoneTracker) error {
	data, err := json.Marshal(zt)
	if err != nil {
		return err
	}
	_, err = cs.store.Put(&api.KVPair{Key: cs.backingKey, Value: data}, nil)
	return err
}
