package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"

	"github.com/hashicorp/consul/api"
)

type LoadSaver interface {
	Save(*UserDB) error
	Load(*UserDB) error
}

type FileStore struct {
	backingDatabase string
}

func NewFileStore(dbFile, dbInit string) (*FileStore, error) {
	info, err := os.Stat(dbFile)
	if err != nil {
		info, err = os.Stat(dbInit)
		if err != nil {
			return nil, err
		}
		cpCmd := exec.Command("cp", dbInit, dbFile)
		err = cpCmd.Run()
		if err != nil {
			return nil, err
		}
		info, err = os.Stat(dbFile)
		if err != nil {
			return nil, err
		}
	}
	if !info.Mode().IsRegular() {
		return nil, fmt.Errorf("%s is not a regular file", dbFile)
	}
	return &FileStore{backingDatabase: dbFile}, nil
}

func (fs *FileStore) Save(dt *UserDB) error {
	data, err := json.Marshal(dt)
	if err != nil {
		return err
	}
	if err := ioutil.WriteFile(fs.backingDatabase, data, 0700); err != nil {
		return err
	}
	return nil
}

func (fs *FileStore) Load(dt *UserDB) error {
	data, err := ioutil.ReadFile(fs.backingDatabase)
	if err != nil {
		return err
	}
	return json.Unmarshal(data, dt)
}

type ConsulStore struct {
	store      *api.KV
	backingKey string
}

func NewConsulStore(key, dbInit string) (*ConsulStore, error) {
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
		data, err := ioutil.ReadFile(dbInit)
		if err != nil {
			return nil, err
		}
		_, err = store.Put(&api.KVPair{Key: key, Value: data}, nil)
		if err != nil {
			return nil, err
		}
	}
	return &ConsulStore{store: store, backingKey: key}, nil
}

func (cs *ConsulStore) Load(dt *UserDB) error {
	pair, _, err := cs.store.Get(cs.backingKey, nil)
	if err != nil {
		return err
	}
	return json.Unmarshal(pair.Value, dt)
}

func (cs *ConsulStore) Save(dt *UserDB) error {
	data, err := json.Marshal(dt)
	if err != nil {
		return err
	}
	_, err = cs.store.Put(&api.KVPair{Key: cs.backingKey, Value: data}, nil)
	return err
}
