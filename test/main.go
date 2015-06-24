package main

// This is a very simple and stupid test binary for the Crowbar API.
// It should not be relied on, used for inspiration, or really even be looked at.

import (
	"encoding/json"
	crowbar "github.com/VictorLowther/crowbar-api"
	"log"
)

func main() {
	err := crowbar.Session("http://127.0.0.1:3000", "crowbar", "crowbar")
	if err != nil {
		log.Panic(err)
	}

	system := &crowbar.Deployment{Name: "system"}
	err = crowbar.Read(system)
	if err != nil {
		log.Panic(err)
	}
	blob, err := json.Marshal(system)
	if err != nil {
		log.Panic(err)
	}
	log.Print("Deployment")
	log.Print(string(blob))
	log.Print("")
	cak := &crowbar.Attrib{Name: "crowbar-access_keys"}
	err = system.GetAttrib(cak)
	if err != nil {
		log.Panic(err)
	}
	blob, err = json.Marshal(cak)
	if err != nil {
		log.Panic(err)
	}
	log.Print("Attrib")
	log.Print(string(blob))
	log.Print("")
	node := &crowbar.Node{ID: 2}
	err = crowbar.Read(node)
	if err != nil {
		log.Panic(err)
	}
	blob, err = json.Marshal(node)
	if err != nil {
		log.Panic(err)
	}
	log.Print("Node")
	log.Print(string(blob))
	log.Print("")
	noderoles, err := crowbar.NodeRoles()
	if err != nil {
		log.Panic(err)
	}
	blob, err = json.Marshal(noderoles)
	if err != nil {
		log.Panic(err)
	}
	log.Print("NodeRoles")
	log.Print(string(blob))
	log.Print("")
	noderoles, err = node.NodeRoles()
	if err != nil {
		log.Panic(err)
	}
	blob, err = json.Marshal(noderoles)
	if err != nil {
		log.Panic(err)
	}
	log.Print("NodeRoles")
	log.Print(string(blob))
	log.Print("")

	noderole := &crowbar.NodeRole{ID: 1}
	err = crowbar.Read(noderole)
	if err != nil {
		log.Panic(err)
	}
	blob, err = json.Marshal(noderole)
	if err != nil {
		log.Panic(err)
	}
	log.Print("NodeRole")
	log.Print(string(blob))
	log.Print("")
	role := &crowbar.Role{Name: "crowbar-access"}
	err = crowbar.Read(role)
	if err != nil {
		log.Panic(err)
	}
	blob, err = json.Marshal(role)
	if err != nil {
		log.Panic(err)
	}
	log.Print("Role")
	log.Print(string(blob))
	log.Print("")
}
