package main

import (
	"encoding/json"
	"fmt"
	"log"

	"github.com/digitalrebar/rebar-api/client"
)

type runContext struct {
	event        *Event
	rule         *Rule
	foundAttribs map[string]interface{}
	variables    map[string]interface{}
}

func makeContext(e *Event) *runContext {
	return &runContext{
		event:     e,
		variables: make(map[string]interface{}),
	}
}

func (e *runContext) fetchAttribs() error {
	if len(e.rule.WantsAttribs) == 0 {
		return nil
	}
	eventId, _ := e.event.Event.Id()
	var attribSrc client.Attriber
	var attribSrcName string
	if e.event.Node != nil {
		attribSrcName = "node"
		attribSrc = e.event.Node
	} else if e.event.Deployment != nil {
		attribSrcName = "deployment"
		attribSrc = e.event.Deployment
	} else if e.event.Role != nil {
		attribSrcName = "role"
		attribSrc = e.event.Role
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

func (e *runContext) attribsJSON() string {
	buf, err := json.Marshal(e.foundAttribs)
	if err != nil {
		log.Panicf("Error marshalling JSON for attribs")
	}
	return string(buf)
}

// Process the Rules against an event.  This implementation is very
// slow and stupid, and will be replaced if it starts being a
// bottleneck.
func (e *runContext) Process(rules []*Rule) {
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
			e.event.Selector["name"],
			matched)
		if matcherr != nil {
			log.Printf("Rule %s had an error matching event %s: %v",
				rule.Name,
				e.event.Selector["name"],
				matcherr)
		}
		if runerr != nil {
			log.Printf("Rule %s failed to fire properly for event %s: %v",
				rule.Name,
				e.event.Selector["name"],
				runerr)
		}

	}
}
