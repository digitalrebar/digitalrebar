package main

import (
	"encoding/json"
	"log"
	"reflect"

	"github.com/digitalrebar/rebar-api/client"
)

type Event struct {
	Name         string       `json:"event"`
	Node         *client.Node `json:"node"`
	Role         *client.Role `json:"role"`
	rule         *Rule
	foundAttribs map[string]interface{}
	variables    map[string]interface{}
}

func (e *Event) fetchAttribs() error {
	val := reflect.ValueOf(e.Node)
	for val.Kind() == reflect.Ptr {
		val = reflect.Indirect(val)
	}
	if val.Kind() != reflect.Struct {
		log.Panicf("node in Event does not point to a struct! This should never happen")
	}
	e.foundAttribs = map[string]interface{}{}
	for _, attribName := range e.rule.WantsAttribs {
		field := val.FieldByName(attribName)
		if field.IsValid() {
			// The node has this attribute natively, use it.
			e.foundAttribs[attribName] = field.Interface()
		} else {
			// We want an actual attrib, fetch and use it.
			attr, err := client.FetchAttrib(e.Node, attribName, "")
			if err != nil {
				return err
			}
			e.foundAttribs[attribName] = attr.Value
		}
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

func (e *Event) Process(rules []*Rule) {
	for _, rule := range rules {
		e.rule = rule
		if err := e.fetchAttribs(); err != nil {
			log.Printf("Failed to fetch attributes for node %v: %v", e.Node.Name, err)
			continue
		}
		ok, err := rule.Match(e)
		if err != nil {
			log.Printf("Match failed for event %s rule %s: %s", e.Name, rule.Name, err)
			continue
		}
		if !ok {
			continue
		}
		if err := rule.Run(e); err != nil {
			log.Printf("Error running rule %s for event %s: %v", rule.Name, e.Name, err)
		}
	}
}
