package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path"
	"path/filepath"
	"strings"

	consul "github.com/hashicorp/consul/api"
)

type keySaver interface {
	Prefix() string
	Key() string
	OnChange(interface{}) error
	OnDelete() error
}

type storageBackend interface {
	List(keySaver) [][]byte
	Save(keySaver, interface{}) error
	Load(keySaver) error
	Delete(keySaver) error
}

type FileBackend string

func NewFileBackend(path string) (FileBackend, error) {
	fullPath, err := filepath.Abs(filepath.Clean(path))
	if err != nil {
		return "", err
	}
	if err := os.MkdirAll(fullPath, 0755); err != nil {
		return "", err
	}
	return FileBackend(fullPath), nil
}

func (f FileBackend) mkThingPath(thing keySaver) string {
	fullPath := filepath.Clean(filepath.Join(string(f), thing.Prefix()))
	if err := os.MkdirAll(fullPath, 0755); err != nil {
		log.Fatalf("file: Cannot create %s: %v", fullPath, err)
	}
	return fullPath
}

func (f FileBackend) mkThingName(thing keySaver) string {
	return filepath.Clean(filepath.Join(string(f), thing.Key())) + ".json"
}

func (f FileBackend) List(thing keySaver) [][]byte {
	dir := f.mkThingPath(thing)
	file, err := os.Open(dir)
	if err != nil {
		log.Fatalf("file: Failed to open dir %s: %v", dir, err)
	}
	names, err := file.Readdirnames(0)
	if err != nil {
		log.Fatalf("file: Failed to get listing for dir %s: %v", dir, err)
	}
	log.Printf("%+v", names)
	res := make([][]byte, 0, len(names))
	for _, name := range names {
		if !strings.HasSuffix(name, ".json") {
			continue
		}
		fullName := filepath.Join(dir, name)
		buf, err := ioutil.ReadFile(fullName)
		if err != nil {
			log.Fatalf("file: Failed to read info for %s: %v", fullName, err)
		}
		res = append(res, buf)
	}
	return res
}

func (f FileBackend) Load(thing keySaver) error {
	fullName := f.mkThingName(thing)
	buf, err := ioutil.ReadFile(fullName)
	if err != nil {
		return fmt.Errorf("file: Failed to read %s: %v", fullName, err)
	}
	return json.Unmarshal(buf, &thing)
}

func (f FileBackend) Save(newThing keySaver, oldThing interface{}) error {
	f.mkThingPath(newThing)
	if err := newThing.OnChange(oldThing); err != nil {
		return err
	}
	fullPath := f.mkThingName(newThing)
	file, err := os.Create(fullPath)
	if err != nil {
		return fmt.Errorf("file: Failed to open thing %s: %v", fullPath, err)
	}
	enc := json.NewEncoder(file)
	if err := enc.Encode(newThing); err != nil {
		os.Remove(fullPath)
		file.Close()
		return fmt.Errorf("file: Failed to save %s: %v", fullPath, err)
	}
	file.Sync()
	file.Close()
	return nil
}

func (f FileBackend) Delete(thing keySaver) error {
	return os.Remove(f.mkThingName(thing))
}

type ConsulBackend struct {
	kv      *consul.KV
	baseKey string
}

func (cb *ConsulBackend) makePrefix(thing keySaver) string {
	return path.Clean(path.Join(cb.baseKey, thing.Prefix()))
}

func (cb *ConsulBackend) makeKey(thing keySaver) string {
	return path.Clean(path.Join(cb.baseKey, thing.Key()))
}

func NewConsulBackend(baseKey string) (*ConsulBackend, error) {
	client, err := consul.NewClient(consul.DefaultConfig())
	if err != nil {
		return nil, err
	}
	backend := &ConsulBackend{
		kv:      client.KV(),
		baseKey: baseKey,
	}
	return backend, nil
}

func (cb *ConsulBackend) List(thing keySaver) [][]byte {
	keypairs, _, err := cb.kv.List(cb.makePrefix(thing), nil)
	if err != nil {
		return [][]byte{}
	}
	res := make([][]byte, len(keypairs))
	for i, kp := range keypairs {
		res[i] = kp.Value
	}
	return res
}

func (cb *ConsulBackend) Save(newThing keySaver, oldThing interface{}) error {
	if err := newThing.OnChange(oldThing); err != nil {
		return err
	}
	buf, err := json.Marshal(newThing)
	if err != nil {
		return fmt.Errorf("consul: Failed to marshal %+v: %v", newThing, err)
	}
	kp := &consul.KVPair{Value: buf, Key: cb.makeKey(newThing)}
	if _, err := cb.kv.Put(kp, nil); err != nil {
		return fmt.Errorf("consul: Failed to save %s: %v", kp.Key, err)
	}
	return err
}

func (cb *ConsulBackend) Load(s keySaver) error {
	key := cb.makeKey(s)
	kp, _, err := cb.kv.Get(key, nil)
	if err != nil {
		return fmt.Errorf("consul: Communication failure: %v", err)
	} else if kp == nil {
		return fmt.Errorf("consul: Failed to load %v", key)
	}
	if err := json.Unmarshal(kp.Value, &s); err != nil {
		return fmt.Errorf("consul: Failed to unmarshal %s: %v", kp.Key, err)
	}
	return nil
}

func (cb *ConsulBackend) Delete(s keySaver) error {
	if err := s.OnDelete(); err != nil {
		return err
	}
	key := cb.makeKey(s)
	if _, err := cb.kv.Delete(key, nil); err != nil {
		return fmt.Errorf("consul: Failed to delete %v: %v", key, err)
	}
	return nil
}
