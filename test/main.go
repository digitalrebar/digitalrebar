package main
// This is a very simple and stupid test binary for the Crowbar API.
// It should not be relied on, used for inspiration, or really even be looked at.

import (
	crowbar "github.com/VictorLowther/crowbar-api"
	"log"
	"encoding/json"
)

func main () {
	err := crowbar.Session("http://127.0.0.1:3000","crowbar","crowbar")
	if err != nil {
		log.Fatal(err)
	}

	system := &crowbar.Deployment{Name:"system"}
	err = system.Get()
	if err != nil {
		log.Fatal(err)
	}
	blob, err := json.Marshal(system)
	if err != nil {
		log.Fatal(err)
	}
	log.Print(string(blob))
	cak := &crowbar.Attrib{Name:"crowbar-access_keys"}
	err = system.GetAttrib(cak)
	if err != nil {
		log.Fatal(err)
	}
	blob, err = json.Marshal(cak)
	if err != nil {
		log.Fatal(err)
	}
	log.Print(string(blob))
	node := &crowbar.Node{ID:2}
	err = node.Get()
	if err != nil {
		log.Fatal(err)
	}
	blob, err = json.Marshal(node)
	if err != nil {
		log.Fatal(err)
	}
	log.Print(string(blob))
	role := &crowbar.Role{Name:"crowbar-access"}
	err = role.Get()
	if err != nil {
		log.Fatal(err)
	}
	blob, err = json.Marshal(role)
	if err != nil {
		log.Fatal(err)
	}
	log.Print(string(blob))
}
