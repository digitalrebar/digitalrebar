package main

import (
	"encoding/json"
	"fmt"
	"log"

	"github.com/digitalrebar/rebar-api/client"
)

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
	rule              *Rule
	foundAttribs      map[string]interface{}
	variables         map[string]interface{}
}

func (e *Event) fetchAttribs() error {
	if len(e.rule.WantsAttribs) == 0 {
		return nil
	}
	eventId, _ := e.Event.Id()
	var attribSrc client.Attriber
	var attribSrcName string
	if e.Node != nil {
		attribSrcName = "node"
		attribSrc = e.Node
	} else if e.Deployment != nil {
		attribSrcName = "deployment"
		attribSrc = e.Deployment
	} else if e.Role != nil {
		attribSrcName = "role"
		attribSrc = e.Role
	} else {
		return fmt.Errorf("Event %s does not have attrib sources", eventId)
	}
	srcId, _ := attribSrc.Id()
	log.Printf("Using %s %s as attrib source for event %s", attribSrcName, srcId, eventId)
	e.foundAttribs = map[string]interface{}{}
	for _, attribName := range e.rule.WantsAttribs {
		// We want an actual attrib, fetch and use it.
		attr, err := client.FetchAttrib(attribSrc, attribName, "")
		if err != nil {
			return fmt.Errorf("Error fetching attrib %s for event %s", attribName, eventId)
		}
		e.foundAttribs[attribName] = attr.Value
	}
	return nil
}

func (e *Event) attribsJSON() string {
	buf, err := json.Marshal(e.foundAttribs)
	if err != nil {
		log.Panicf("Error marshalling JSON for attribs")
	}
	return string(buf)
}

// Process the Rules against an event.  This implementation is very
// slow and stupid, and will be replaced if it starts being a
// bottleneck.
func (e *Event) Process(rules []*Rule) {
	ruleLock.Lock()
	defer ruleLock.Unlock()
	for _, rule := range rules {
		e.rule = rule
		if err := e.fetchAttribs(); err != nil {
			log.Printf("Error fetching attribs: %v", err)
			continue
		}
		matched, matcherr, runerr := rule.Fire(e)
		log.Printf("Rule %s matched event %s: %v",
			rule.Name,
			e.Selector["name"],
			matched)
		if matcherr != nil {
			log.Printf("Rule %s had an error matching event %s: %v",
				rule.Name,
				e.Selector["name"],
				matcherr)
		}
		if runerr != nil {
			log.Printf("Rule %s failed to fire properly for event %s: %v",
				rule.Name,
				e.Selector["name"],
				runerr)
		}

	}
}
