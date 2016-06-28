package main

/*
Copyright (c) 2016, Rackn Inc.
Licensed under the terms of the Digital Rebar License.
See LICENSE.md at the top of this repository for more information.
*/

import "github.com/digitalrebar/rebar-api/client"

// Event is what is recieved from the DigitalRebar core whenever
// something of interest occurs.  The current definition is subject to
// change as the needs of the Classifier grow.
type Event struct {
	Selector          map[string]string         `json:"selector"`
	Event             *client.Event             `json:"event"`
	Node              *client.Node              `json:"node"`
	Role              *client.Role              `json:"role"`
	NodeRole          *client.NodeRole          `json:"node_role"`
	Deployment        *client.Deployment        `json:"deployment"`
	DeploymentRole    *client.DeploymentRole    `json:"deployment_role"`
	Network           *client.Network           `json:"network"`
	NetworkAllocation *client.NetworkAllocation `json:"network_allocation"`
	NetworkRange      *client.NetworkRange      `json:"network_range"`
	NetworkRouter     *client.NetworkRouter     `json:"network_router"`
}
