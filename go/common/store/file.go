package store

import (
	"io/ioutil"
	"net/url"
	"os"
	"path/filepath"
	"strings"
)

type FileStore string

func (f FileStore) name(n string) string {
	return filepath.Join(string(f), url.QueryEscape(n)) + ".json"
}

func NewFileBackend(path string) (FileStore, error) {
	fullPath, err := filepath.Abs(filepath.Clean(path))
	if err != nil {
		return "", err
	}
	if err := os.MkdirAll(fullPath, 0755); err != nil {
		return "", err
	}
	return FileStore(fullPath), nil
}

func (f FileStore) Sub(path string) SimpleStore {
	return FileStore(filepath.Join(string(f), path))
}

func (f FileStore) Keys() ([]string, error) {
	d, err := os.Open(string(f))
	if err != nil {
		return nil, err
	}
	names, err := d.Readdirnames(0)
	if err != nil {
		return nil, err
	}
	res := make([]string, 0, len(names))
	for _, name := range names {
		if !strings.HasSuffix(name, ".json") {
			continue
		}
		n, err := url.QueryUnescape(strings.TrimSuffix(name, ".json"))
		if err != nil {
			return nil, err
		}
		res = append(res, n)
	}
	return res[:], nil
}

func (f FileStore) Load(key string) ([]byte, error) {
	return ioutil.ReadFile(f.name(key))
}

func (f FileStore) Save(key string, val []byte) error {
	file, err := os.Create(f.name(key))
	if err != nil {
		return err
	}
	defer file.Close()
	_, err = file.Write(val)
	if err != nil {
		os.Remove(file.Name())
		return err
	}
	file.Sync()
	return nil
}

func (f FileStore) Remove(key string) error {
	return os.Remove(f.name(key))
}
