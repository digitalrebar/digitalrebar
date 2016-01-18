package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
)

type LoadSaver interface {
	Save(*DataTracker) error
	Load(*DataTracker) error
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

func (fs *FileStore) Save(dt *DataTracker) error {
	dt.Lock()
	defer dt.Unlock()
	data, err := json.Marshal(dt)
	if err != nil {
		return err
	}
	if err := ioutil.WriteFile(fs.backingDatabase, data, 0700); err != nil {
		return err
	}
	return nil
}

func (fs *FileStore) Load(dt *DataTracker) error {
	dt.Lock()
	defer dt.Unlock()
	data, err := ioutil.ReadFile(fs.backingDatabase)
	if err != nil {
		return err
	}
	return json.Unmarshal(data, dt)
}
