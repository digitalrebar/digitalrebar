package store

import "sync"

// MemoryStore provides an in-memory implementation of SimpleStore
// for testing purposes
type SimpleMemoryStore struct {
	sync.RWMutex
	v map[string][]byte
}

func NewSimpleMemoryStore() *SimpleMemoryStore {
	return &SimpleMemoryStore{v: make(map[string][]byte)}
}

func (m *SimpleMemoryStore) Sub(loc string) (SimpleStore, error) {
	return NewSimpleMemoryStore(), nil
}

func (m *SimpleMemoryStore) Keys() ([]string, error) {
	m.RLock()
	res := make([]string, 0, len(m.v))
	for k := range m.v {
		res = append(res, k)
	}
	m.RUnlock()
	return res, nil
}

func (m *SimpleMemoryStore) List() ([][]byte, error) {
	m.RLock()
	res := make([][]byte, 0, len(m.v))
	for _, v := range m.v {
		val := make([]byte, len(v))
		copy(val, v)
		res = append(res, val)
	}
	m.RUnlock()
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
	_, ok := m.v[key]
	if ok {
		delete(m.v, key)
		m.Unlock()
		return nil
	}
	m.Unlock()
	return NotFound(key)
}
