package engine

import (
	"encoding/json"
	"fmt"
	"log"
	"reflect"
	"sync"

	"github.com/digitalrebar/go-common/event"
	"github.com/digitalrebar/go-common/store"
	"github.com/digitalrebar/rebar-api/api"
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
	selector    event.Selector
	ruleIndexes map[string][]int
}

// Engine holds all the necessary information to run RuleSets.
type Engine struct {
	sync.RWMutex
	Client         *api.Client
	Sink           *event.Sink
	backingStore   store.SimpleStore
	trusted        bool
	ruleSets       map[string]*RuleSet
	scriptEnv      map[string]string
	eventEndpoint  string
	Debug          bool
	eventSelectors []etInvoker
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
func NewEngine(backingStore store.SimpleStore,
	client *api.Client,
	trusted bool,
	scriptEnv map[string]string) (*Engine, error) {
	res := &Engine{
		backingStore:   backingStore,
		Client:         client,
		trusted:        trusted,
		scriptEnv:      scriptEnv,
		ruleSets:       map[string]*RuleSet{},
		eventSelectors: []etInvoker{},
	}

	// Load rules
	keys, err := backingStore.Keys()
	if err != nil {
		return nil, err
	}
	for _, key := range keys {
		buf, err := backingStore.Load(key)
		if err != nil {
			return nil, err
		}
		rs := &RuleSet{}
		if err := json.Unmarshal(buf, rs); err != nil {
			return nil, err
		}
		res.ruleSets[key] = rs
	}

	// Compile them
	for _, rs := range res.ruleSets {
		rs.compile(res)
	}
	return res, nil
}

func (e *Engine) RegisterSink(URL string) error {
	sink, err := event.NewSink(e.Client, URL, e)
	if err != nil {
		return err
	}
	e.Sink = sink
	e.updateSelectors()
	return nil
}

// Called whenever we add/update/remove RuleSets. It makes sure
// that the proper EventSelectors are registered with DigitalRebar.
func (e *Engine) updateSelectors() {
	// If there is no rebarEndpoint, we are in testing mode.
	if e.eventEndpoint == "" {
		return
	}
	selectors := []event.Selector{}
	for _, rs := range e.ruleSets {
		for i := range rs.Rules {
			rule := &rs.Rules[i]
			for _, es := range rule.EventSelectors {
				found := false
				for k := range e.eventSelectors {
					evt := &e.eventSelectors[k]
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
					selectors = append(selectors, event.Selector(es))
				}
			}
		}
	}
	if err := e.Sink.SetSelectors(e.Client, selectors); err != nil {
		log.Fatalf("Failed to set event selectors: %v", err)
	}
}

// Stop gracefully shuts down, deregistering all our selectors and sinks
func (e *Engine) Stop() {
	// Note that the lock is not released.
	e.Lock()
	e.Sink.Stop(e.Client)
}

// Update e.eventselectors whenever a Ruleset is added or changed.
func (e *Engine) updateRules(rs RuleSet) (RuleSet, error) {
	err := rs.compile(e)
	if err != nil {
		return rs, err
	}
	rs.Version = uuid.NewRandom()
	buf, err := json.Marshal(rs)
	if err != nil {
		return rs, err
	}
	if err := e.backingStore.Save(rs.Name, buf); err != nil {
		return rs, err
	}
	e.ruleSets[rs.Name] = &rs
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
	e.backingStore.Remove(name)
	return nil
}

func (e *Engine) runRules(toRun []ctx, evt *event.Event) {
	for _, v := range toRun {
		ctx := &RunContext{
			Engine:  e,
			Evt:     evt,
			Vars:    make(map[string]interface{}),
			ruleset: &v.ruleSet,
		}
		if e.trusted {
			if v.ruleSet.Username != "" {
				log.Panicf("Cannot happen: trusted client with no username")
			}
			if c, err := api.TrustedSession(e.Client.URL, v.ruleSet.Username); err != nil {
				log.Panicf("Failed to establis trusted session impersonating %s", v.ruleSet.Username)
			} else {
				ctx.Client = c
			}
		} else {
			ctx.Client = e.Client
		}
		ctx.Vars["eventType"] = evt.Selector["event"]
		for _, ri := range v.ruleIndexes {
			ctx.processFrom(ri)
		}
	}
}

// HandleEvent should be called with an Event for the Engine to process.
func (e *Engine) HandleEvent(evt *event.Event) error {
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
	return nil
}
