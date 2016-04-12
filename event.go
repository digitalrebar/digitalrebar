package main

import (
	"github.com/digitalrebar/rebar-api/client"
)

type Event struct {
	Name string       `json:"event"`
	Node *client.Node `json:"node"`
	Role *client.Role `json:"role"`
}
