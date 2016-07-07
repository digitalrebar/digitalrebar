package engine

import (
	"fmt"
	"log"
	"reflect"
	"sync"

	"github.com/digitalrebar/rebar-api/client"
	"github.com/pborman/uuid"
)

// This is effectively condensed cache/dispatch structure for
// individual rule.EventSelector entries.  It is updated
// whenever a RuleSet is added, modified, or deleted,
// and allows for fast dispatch of Events directly to relavent Rules
// Instead of searching all the Rules to figure out which ones apply to
// any given Event, we search through this instead, which by desigh has
// exactly one entry per unique Selector we care about.
type etInvoker struct {
	selector    EventSelector
	ruleIndexes map[string][]int
}

// Engine holds all the necessary information to run RuleSets.
type Engine struct {
	sync.RWMutex
	ruleSets           map[string]*RuleSet
	eventEndpoint      string
	rebarEndpoint      string
	username, password string
	Debug              bool
	eventSelectors     []etInvoker
}

// NewEngine creates a new Engine for running Rulesets.
// The parameters are as follows:
//
// listen: the URL that the Engine listens to events at.
// This is needed because
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

// Called whenever we add/update/remove RuleSets. It makes sure
// that the proper EventSelectors are registered with DigitalRebar.
func (e *Engine) updateSelectors() {
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
	// Note that the lock is not released.
	e.Lock()
	for rs := range e.ruleSets {
		e.deleteRuleSet(rs)
	}
	es, _ := e.eventSink()
	client.Destroy(es)
}

// Update e.eventselectors whenever a Ruleset is added or changed.
func (e *Engine) updateRules(rs RuleSet) (RuleSet, error) {
	err := rs.compile(e)
	if err != nil {
		return rs, err
	}
	rs.Version = uuid.NewRandom()
	e.ruleSets[rs.Name] = &rs
	for i := range rs.Rules {
		rule := &rs.Rules[i]
		for _, es := range rule.EventSelectors {
			found := false
			for k := range e.eventSelectors {
				evt := &rs.engine.eventSelectors[k]
				if reflect.DeepEqual(es, evt.selector) {
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

				e.eventSelectors = append(e.eventSelectors, etInvoker{es, map[string][]int{rs.Name: []int{i}}})
			}
		}
	}
	e.updateSelectors()
	return rs, nil
}

// RuleSets returns a slice of RuleSets that the Engine will use to process
// incoming Events.  The Engine must be read-locked or exclusively locked
// to call this.
func (e *Engine) RuleSets() []RuleSet {
	e.RLock()
	res := make([]RuleSet, 0, len(e.ruleSets))
	for _, rs := range e.ruleSets {
		res = append(res, *rs)
	}
	e.RUnlock()
	return res
}

// RuleSet looks up a RuleSet by name and either returns it and true
// or returns an empty RuleSet and false.
func (e *Engine) RuleSet(name string) (RuleSet, bool) {
	e.RLock()
	rs, ok := e.ruleSets[name]
	e.RUnlock()
	if ok {
		return *rs, ok
	}
	return RuleSet{}, ok
}

// AddRuleSet adds a RuleSet to the Engine.
func (e *Engine) AddRuleSet(rs RuleSet) (RuleSet, error) {
	e.Lock()
	defer e.Unlock()
	if _, ok := e.ruleSets[rs.Name]; ok {
		return rs, fmt.Errorf("RuleSet %s already exists, and duplicates are not allowed", rs.Name)
	}
	log.Printf("Adding ruleset %s", rs.Name)
	return e.updateRules(rs)
}

// UpdateRuleSet updates the Engine with the new RuleSet.
func (e *Engine) UpdateRuleSet(rs RuleSet) (RuleSet, error) {
	e.Lock()
	defer e.Unlock()
	currentRuleSet, ok := e.ruleSets[rs.Name]
	if !ok {
		return rs, fmt.Errorf("RuleSet %s is not a member of this engine", rs.Name)
	}
	if !uuid.Equal(currentRuleSet.Version, rs.Version) {
		return rs, fmt.Errorf("RuleSet %s version mismatch: ours %v, theirs %v",
			rs.Name,
			currentRuleSet.Version,
			rs.Version)
	}

	log.Printf("Updating ruleset %s", rs.Name)
	return e.updateRules(rs)
}

func (e *Engine) deleteRuleSet(name string) {
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
	e.updateSelectors()
}

// DeleteRuleSet deletes the named RuleSet from the Engine.
func (e *Engine) DeleteRuleSet(name string, version uuid.UUID) error {
	log.Printf("Deleting ruleset %s", name)
	e.Lock()
	defer e.Unlock()
	rs, ok := e.ruleSets[name]
	if !ok {
		return fmt.Errorf("RuleSet %s does not exist", name)
	}
	if !uuid.Equal(version, rs.Version) {
		return fmt.Errorf("Ruleset %s version mismatch: ours %v, theirs %v",
			rs.Name,
			rs.Version,
			version)
	}
	e.deleteRuleSet(name)
	return nil
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

// HandleEvent should be called with an Event for the Engine to process.
func (e *Engine) HandleEvent(evt *Event) {
	e.RLock()
	toRun := []ctx{}
	for _, invoker := range e.eventSelectors {
		if !invoker.selector.Match(evt.Selector) {
			continue
		}
		for k, v := range invoker.ruleIndexes {
			// Skip inactive rulesets.
			if rs, ok := e.ruleSets[k]; !ok || !rs.Active {
				continue
			}
			// Make sure we save a copy of the ruleset to run, instead of a
			// pointer to it.  This makes async running of rules from a ruleset
			// not race with potential API updates of that ruleset.
			toRun = append(toRun, ctx{*e.ruleSets[k], v})
		}
	}
	e.RUnlock()
	e.runRules(toRun, evt)
}
