package store

import (
	"path/filepath"

	"github.com/boltdb/bolt"
)

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

func (b *SimpleLocalStore) Sub(loc string) SimpleStore {
	return &SimpleLocalStore{db: b.db, bucket: []byte(loc)}
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
