package store

import (
	"path"
	"strings"

	consul "github.com/hashicorp/consul/api"
)

type SimpleConsulStore struct {
	kv      *consul.KV
	baseKey string
}

func NewSimpleConsulStore(c *consul.Client, prefix string) (*SimpleConsulStore, error) {
	return &SimpleConsulStore{kv: c.KV(), baseKey: prefix}, nil
}

func (b *SimpleConsulStore) Sub(prefix string) SimpleStore {
	return &SimpleConsulStore{kv: b.kv, baseKey: path.Join(b.baseKey, prefix)}
}

func (b *SimpleConsulStore) finalKey(k string) string {
	return path.Clean(path.Join(b.baseKey, k))
}

func (b *SimpleConsulStore) Keys() ([]string, error) {
	keys, _, err := b.kv.Keys(b.baseKey, "", nil)
	res := make([]string, len(keys))
	for i := range keys {
		res[i] = strings.TrimPrefix(keys[i], b.baseKey+"/")
	}
	return res, err
}

func (b *SimpleConsulStore) Load(key string) ([]byte, error) {
	val, _, err := b.kv.Get(b.finalKey(key), nil)
	if val == nil {
		return nil, NotFound(key)
	}
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
