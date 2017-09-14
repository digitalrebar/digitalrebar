package main

import (
	"flag"
	"fmt"
	"log"

	"github.com/digitalrebar/digitalrebar/go/common/store"
	consul "github.com/hashicorp/consul/api"
)

func open(storeType, storeLoc string) (store.SimpleStore, error) {
	switch storeType {
	case "consul":
		c, err := consul.NewClient(consul.DefaultConfig())
		if err != nil {
			log.Fatalf("Error talking to Consul: %v", err)
		}
		return store.NewSimpleConsulStore(c, storeLoc)
	case "bolt", "local":
		return store.NewSimpleLocalStore(storeLoc)
	case "file", "directory":
		return store.NewFileBackend(storeLoc)
	default:
		return nil, fmt.Errorf("Unknown store type '%s'", storeType)
	}
}

func main() {
	var srcLoc, destLoc, src, dest string
	flag.StringVar(&src, "src", "", "Source store type")
	flag.StringVar(&dest, "dest", "", "Destination store type")
	flag.StringVar(&srcLoc, "srcLoc", "", "Source store location")
	flag.StringVar(&destLoc, "destLoc", "", "Destination store location")
	flag.Parse()
	from, err := open(src, srcLoc)
	if err != nil {
		log.Fatalf("Error opening source store: %v", err)
	}
	to, err := open(dest, destLoc)
	if err != nil {
		log.Fatalf("Error opening destination store: %v", err)
	}
	keys, err := from.Keys()
	if err != nil {
		log.Fatalf("Error getting keys from destination store")
	}
	for _, key := range keys {
		buf, err := from.Load(key)
		if err != nil {
			log.Fatalf("Error loading key %s data from source: %v", key, err)
		}
		err = to.Save(key, buf)
		if err != nil {
			log.Fatalf("Error saving key %s data to destination: %v", key, err)
		}
		log.Printf("Key %s: %d bytes copied from %s to %s", key, len(buf), src, dest)
	}
	log.Println("All data moved.")
}
