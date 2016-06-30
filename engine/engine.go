package engine

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"reflect"
	"strings"
	"sync"

	"github.com/digitalrebar/rebar-api/client"
)

// Engine holds all the necessary information to run RuleSets.
type Engine struct {
	sync.RWMutex
	ruleSets           map[string]*RuleSet
	eventEndpoint      string
	apiEndpoint        string
	rebarEndpoint      string
	username, password string
	Debug              bool

	eventSelectors []etInvoker
}

// NewEngine creates a new Engine for running Rulesets.
// The parameters are as follows:
//
// listen: the local address:port that this Engine should accept
// incoming Events and API requests at.  Right now, the Engine
// will listen for Events at http://address:port/events and API
// requests at http://address:port/api/
//
// rebarEndpoint: the URL to contact the Rebar API at.
//
// username, password: The username and password to use to communicate with Rebar.
func NewEngine(listen, rebarEndpoint, username, password string) (*Engine, error) {
	res := &Engine{
		rebarEndpoint:  rebarEndpoint,
		username:       username,
		password:       password,
		eventEndpoint:  fmt.Sprintf("http://%s/events", listen),
		apiEndpoint:    fmt.Sprintf("http://%s/api/", listen),
		ruleSets:       map[string]*RuleSet{},
		eventSelectors: []etInvoker{},
	}
	if err := res.registerSink(); err != nil {
		return nil, err
	}
	return res, nil
}

func (e *Engine) eventSink() (*client.EventSink, error) {
	// If there is no rebarEndpoint, we are in testing mode.
	if e.rebarEndpoint == "" {
		return nil, nil
	}
	// Register us as an event sink, if not already registered.
	res := &client.EventSink{}
	matcher := map[string]interface{}{"endpoint": e.eventEndpoint}
	matches := []*client.EventSink{}
	if err := client.Match(res.ApiName(), matcher, &matches); err != nil {
		return nil, err
	}
	if len(matches) == 0 {
		return nil, nil
	}
	if len(matches) > 1 {
		return nil, fmt.Errorf("Multiple event sinks present for %s, cannot happen!",
			e.eventEndpoint)
	}
	return matches[0], nil
}

func (e *Engine) registerSink() error {
	// If there is no rebarEndpoint, we are in testing mode.
	if e.rebarEndpoint == "" {
		return nil
	}
	sink, err := e.eventSink()
	if err != nil {
		return err
	}
	if sink != nil {
		log.Printf("Already listening at %s", e.eventEndpoint)
		return nil
	}
	sink = &client.EventSink{}
	sink.Endpoint = e.eventEndpoint
	log.Printf("Creating new event sink at %s", e.eventEndpoint)
	return client.BaseCreate(sink)
}

func (e *Engine) updateSinks() {
	// If there is no rebarEndpoint, we are in testing mode.
	if e.rebarEndpoint == "" {
		return
	}
	sink, err := e.eventSink()
	if err != nil {
		log.Fatalf("No event sink available for %s", e.eventEndpoint)
	}
	matcher := map[string]interface{}{"event_sink_id": sink.ID}
	matches := []*client.EventSelector{}
	selector := &client.EventSelector{}
	if err := client.Match(selector.ApiName(), matcher, &matches); err != nil {
		log.Fatalf("Error getting event selectors: %v", err)
	}

	// Build lists of add, delete, already_present
	add := []EventSelector{}
	delete := []*client.EventSelector{}
	alreadyPresent := []*client.EventSelector{}

	// Negative Intersection new with old to get adds
	for _, v := range e.eventSelectors {
		log.Printf("Event selector %#v:", v.selector)
		// Test to see if this selector is in our list.
		doit := true
		for _, e := range matches {
			if reflect.DeepEqual(e.Selector, v.selector.forRebar()) {
				doit = false
				alreadyPresent = append(alreadyPresent, e)
				break
			}
		}
		if doit {
			add = append(add, v.selector)
		}
	}

	// Negative Intersection old with new to get deletes
	for _, v := range matches {
		found := false
		for _, e := range alreadyPresent {
			if v == e {
				found = true
				break
			}
		}

		if !found {
			delete = append(delete, v)
		}
	}

	// Add new selectors
	for _, v := range add {
		selector = &client.EventSelector{}
		selector.EventSinkID = sink.ID
		selector.Selector = v.forRebar()
		log.Printf("Creating new event selector, %v", v)
		if err := client.BaseCreate(selector); err != nil {
			log.Fatalf("Error creating event selector: %v", err)
		}
	}

	// Delete old selectors
	for _, v := range delete {
		log.Printf("Deleting old event selector, %v", v)
		if err := client.Destroy(v); err != nil {
			log.Fatalf("Error destroying event selector: %v", err)
		}
	}
}

// Stop gracefully shuts down, deregistering all our selectors and sinks
func (e *Engine) Stop() {
	e.Lock()
	for rs := range e.ruleSets {
		e.DeleteRuleSet(rs)
	}
	es, _ := e.eventSink()
	client.Destroy(es)
}

func (e *Engine) updateRules(rs RuleSet) error {
	err := rs.compile(e)
	if err != nil {
		return err
	}
	e.ruleSets[rs.Name] = &rs
	for i := range rs.Rules {
		rule := &rs.Rules[i]
		for _, e := range rule.EventSelectors {
			found := false
			for k := range rs.engine.eventSelectors {
				evt := &rs.engine.eventSelectors[k]
				if reflect.DeepEqual(e, evt.selector) {
					found = true
					if _, ok := evt.ruleIndexes[rs.Name]; ok {
						log.Printf("Adding ruleset %s rule %d to selector %v", rs.Name, i, evt.selector)
						evt.ruleIndexes[rs.Name] = append(evt.ruleIndexes[rs.Name], i)
					} else {
						log.Printf("Adding ruleset %s rule %d to selector %v", rs.Name, i, evt.selector)
						evt.ruleIndexes = map[string][]int{rs.Name: []int{i}}
					}
				}
				if found {
					break
				}
			}
			if !found {
				log.Printf("Creating selector %v for ruleset %s rule %d", e, rs.Name, i)

				rs.engine.eventSelectors = append(rs.engine.eventSelectors, etInvoker{e, map[string][]int{rs.Name: []int{i}}})
			}
		}
	}
	e.updateSinks()
	return nil
}

// RuleSets returns a slice of RuleSets that the Engine will use to process
// incoming Events.  The Engine must be read-locked or exclusively locked
// to call this.
func (e *Engine) RuleSets() []RuleSet {
	res := make([]RuleSet, 0, len(e.ruleSets))
	for _, rs := range e.ruleSets {
		res = append(res, *rs)
	}
	return res
}

// AddRuleSet adds a RuleSet to the Engine.
// The Engine must be exclusively locked to call this.
func (e *Engine) AddRuleSet(rs RuleSet) error {
	if _, ok := e.ruleSets[rs.Name]; ok {
		return fmt.Errorf("RuleSet %s already exists, and duplicates are not allowed", rs.Name)
	}
	log.Printf("Adding ruleset %s", rs.Name)
	return e.updateRules(rs)
}

// UpdateRuleSet updates the Engine with the new RuleSet.
// The Engine must be exclusively locked to call this.
func (e *Engine) UpdateRuleSet(rs RuleSet) error {
	if _, ok := e.ruleSets[rs.Name]; !ok {
		return fmt.Errorf("RuleSet %s is not a member of this engine", rs.Name)
	}
	log.Printf("Updating ruleset %s", rs.Name)
	return e.updateRules(rs)
}

// DeleteRuleSet deletes the named RuleSet from the Engine.
// The Engine must ne exclusively locked to call this.
func (e *Engine) DeleteRuleSet(name string) {
	log.Printf("Deleting ruleset %s", name)
	delete(e.ruleSets, name)
	// Process selectors for this engine to drop ones that
	// no longer have references
	newSelectors := []etInvoker{}
	for _, et := range e.eventSelectors {
		delete(et.ruleIndexes, name)
		if len(et.ruleIndexes) == 0 {
			continue
		}
		newSelectors = append(newSelectors, et)
	}
	e.eventSelectors = newSelectors
}

func (e *Engine) runRules(toRun []ctx, evt *Event) {
	for _, v := range toRun {
		ctx := &RunContext{
			Engine:  e,
			Evt:     evt,
			Vars:    make(map[string]interface{}),
			ruleset: &v.ruleSet,
		}
		ctx.Vars["eventType"] = evt.Selector["event"]
		for _, ri := range v.ruleIndexes {
			ctx.processFrom(ri)
		}
	}
}

func (e *Engine) handleEvent(w http.ResponseWriter, req *http.Request) {
	log.Printf("Got request from %v", req.RemoteAddr)
	evt := &Event{}
	body, err := ioutil.ReadAll(req.Body)
	if err != nil {
		log.Printf("Error reading body: %v", err)
		w.WriteHeader(http.StatusExpectationFailed)
		return
	}
	req.Body.Close()
	if err := json.Unmarshal(body, evt); err != nil {
		log.Printf("Error decoding body: %v", err)
		log.Printf("Invalid body: %s", string(body))
		w.WriteHeader(http.StatusExpectationFailed)
		return
	}
	e.RLock()
	toRun := []ctx{}
	for _, invoker := range e.eventSelectors {
		if !invoker.selector.Match(evt.Selector) {
			continue
		}
		for k, v := range invoker.ruleIndexes {
			toRun = append(toRun, ctx{*e.ruleSets[k], v})
		}
	}
	e.RUnlock()
	if runSync, ok := evt.Selector["sync"]; runSync == "true" && ok {
		e.runRules(toRun, evt)
		w.WriteHeader(http.StatusAccepted)
	} else {
		w.WriteHeader(http.StatusAccepted)
		go e.runRules(toRun, evt)
	}
}

func (e *Engine) handleAPI(w http.ResponseWriter, req *http.Request) {
	w.WriteHeader(http.StatusNotImplemented)
}

// ServeHTTP allows an Engine to act as an HTTP endpoint for handling
// incoming events and API requests.
//
// Note that something else must set up the required http server
// infrastructure, and right now we assume we are at the top of the request path.
func (e *Engine) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	log.Printf("Recieved call: %s %s", req.Method, req.RequestURI)
	if req.Method == "POST" && req.URL.Path == "/events" {
		e.handleEvent(w, req)
	} else if strings.HasPrefix(req.URL.Path, "/api/") {
		e.handleAPI(w, req)
	} else {
		w.WriteHeader(http.StatusBadRequest)
	}
}
