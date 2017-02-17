package main

import "github.com/digitalrebar/digitalrebar/go/common/store"

type keySaver interface {
	store.KeySaver
	typeName() string
	tenantId() int
	setTenantId(int)
}

func registerBackends(s store.SimpleStore) {
	backendMux.Lock()
	defer backendMux.Unlock()
	t := &Template{}
	b := &BootEnv{}
	m := &Machine{}
	prefixes := []string{t.Prefix(), b.Prefix(), m.Prefix()}
	for _, p := range prefixes {
		b, err := s.Sub(p)
		if err != nil {
			logger.Fatalf("%s: Error creating substore: %v", p, err)
		}
		backends[p] = b
	}
}

func getBackend(t store.KeySaver) store.SimpleStore {
	backendMux.Lock()
	defer backendMux.Unlock()
	res, ok := backends[t.Prefix()]
	if !ok {
		logger.Fatalf("%s: No registered storage backend!", t.Prefix())
	}
	return res
}
