package main

import (
	"encoding/json"
	"fmt"
	"log"
	"os/exec"
	"reflect"
	"strings"
	"sync"

	"github.com/digitalrebar/rebar-api/client"
)

type Matcher interface {
	Match(*Rule) (bool, error)
}

type ScriptMatch string

func (s ScriptMatch) Match(r *Rule) (bool, error) {
	cmd := exec.Command("/usr/bin/env", "bash", "-x")
	cmd.Stdin = strings.NewReader(fmt.Sprintf("CLASSIFIER_ATTRIBS='%s'\n%s",
		r.foundAttribJSON,
		string(s)))
	out, err := cmd.Output()
	if err == nil {
		log.Printf("Script rule %s ran successfully", r.Name)
		log.Printf("%s", string(out))
		return true, nil
	} else {
		log.Printf("Script rule %s failed", r.Name)
		exitErr, ok := err.(*exec.ExitError)
		if ok {
			log.Printf("%s", string(exitErr.Stderr))
			return false, nil
		} else {
			log.Printf("Failed with error %v", err)
			return false, err
		}
	}
}

type AndMatch struct {
	And []Matcher
}

func (t *AndMatch) Match(rule *Rule) (bool, error) {
	for _, m := range t.And {
		if res, err := m.Match(rule); !res || err != nil {
			return res, err
		}
	}
	return true, nil
}

type OrMatch struct {
	Or []Matcher
}

func (t *OrMatch) Match(rule *Rule) (bool, error) {
	for _, m := range t.Or {
		if res, err := m.Match(rule); res || err != nil {
			return res, err
		}
	}
	return false, nil
}

type NotMatch struct {
	Not Matcher
}

func (t *NotMatch) Match(rule *Rule) (bool, error) {
	res, err := t.Not.Match(rule)
	return !res, err
}

type Action interface {
	Run(*Rule) error
}

type LogAction struct{}

func (l LogAction) Run(r *Rule) error {
	log.Printf("Rule %s matched %s for node %s",
		r.Name,
		r.event.Name,
		r.event.Node.Name)
	return nil
}

type Rule struct {
	sync.Mutex
	Name            string
	Description     string
	WantsAttribs    []string
	Matchers        []Matcher
	Actions         []Action
	event           *Event
	foundAttribs    map[string]interface{}
	foundAttribJSON string // JSON marshalling of attribs
}

func (r *Rule) attribs() error {
	val := reflect.ValueOf(r.event.Node)
	for val.Kind() == reflect.Ptr {
		val = reflect.Indirect(val)
	}
	if val.Kind() != reflect.Struct {
		log.Panicf("node in Rule does not point to a struct! This should never happen")
	}
	for _, attribName := range r.WantsAttribs {
		field := val.FieldByName(attribName)
		if field.IsValid() {
			// The node has this attribute natively, use it.
			r.foundAttribs[attribName] = field.Interface()
		} else {
			// We want an actual attrib, fetch and use it.
			attr, err := client.FetchAttrib(r.event.Node, attribName, "")
			if err != nil {
				return err
			}
			r.foundAttribs[attribName] = attr.Value
		}
	}
	buf, err := json.Marshal(&r.foundAttribs)
	if err != nil {
		return err
	}
	r.foundAttribJSON = string(buf)
	return nil
}

func (r *Rule) Match(event *Event) (bool, error) {
	r.Lock()
	defer r.Unlock()
	r.event = event
	if err := r.attribs(); err != nil {
		return false, err
	}
	res := &AndMatch{And: r.Matchers}
	return res.Match(r)
}

func (r *Rule) Run() error {
	for _, action := range r.Actions {
		if err := action.Run(r); err != nil {
			return err
		}
	}
	return nil
}

func RunRules(rules []*Rule, event *Event) {
	ok := false
	var err error
	for _, rule := range rules {
		ok, err = rule.Match(event)
		if err != nil {
			log.Printf("Error matching rule %s for event %s: %v",
				rule.Name, rule.event.Name, err)
			continue
		}
		if !ok {
			continue
		}
		err = rule.Run()
		if err != nil {
			log.Printf("Error running actions for rule %s for event %s: %v",
				rule.Name, rule.event.Name, err)
		}
	}
}
