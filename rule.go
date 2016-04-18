package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"sync"

	// The nice thing about this YML package is that it parses
	// from yaml to json, so we can use yml and json
	// interchaneable without having to write two sets of struct
	// tags and custom marshalling/unmarshalling code.
	"github.com/ghodss/yaml"
)

// These should eventually be replaced by a rete network or something
// similar if performance in the classifier starts to suffer.
var rules []*Rule
var namedRules = make(map[string]*Rule)
var ruleLock sync.Mutex

// Rule represents a list of Actions that shoudl be taken if an Event matches the Rule.
type Rule struct {
	Name string // Short name of the Rule.  For human consumption
	// only.
	Description string // Descriptive name of the Rule.  Also for
	// human consumption only.
	WantsAttribs []string // List of attributes that should be
	// fetched from Digital Rebar before running the Matchers.
	Matchers []Matcher // List of Matchers that must match for the
	// rule to be considered as matching the Event.  Matchers in
	// this list will be AND'ed together, and if the list is empty
	// then the Rule will match all Events
	MatchActions []Action // List of Actions to run when the Rule
	// matches the Event.
}

// Match runs the rule Matchers.  If all the Matchers match, then the rule is considered an overall Match.
func (r *Rule) Match(e *RunContext) (bool, error) {
	return matchAnd(r.Matchers...)(e)
}

// RunActions runs the rule MatchActions in order.  If any of them return an error, no further MatchActions will be run.
func (r *Rule) RunActions(e *RunContext) error {
	for i, action := range r.MatchActions {
		log.Printf("Rule %s: running action %v", r.Name, i)
		if err := action(e); err != nil {
			return err
		}
	}
	return nil
}

// Fire runs the Matchers against the Event, and runs the MatchActions
// if the Matchers matched.  If the rule failed to Fire, either
// matcherr or runerr will container the error depending on whether
// there was an error determining whether the rule failed to match the
// event or there was an error running the actions.
func (r *Rule) Fire(e *RunContext) (matched bool, matcherr error, runerr error) {
	matched, matcherr = r.Match(e)
	if matcherr != nil || !matched {
		return
	}
	runerr = r.RunActions(e)
	return
}

func loadRules(src string) error {
	buf, err := ioutil.ReadFile(src)
	if err != nil {
		return err
	}
	type fakeRule struct {
		Name         string
		Description  string
		WantsAttribs []string
		Matchers     []map[string]interface{}
		MatchActions []map[string]interface{}
	}
	var newFakeRules []*fakeRule
	if err := yaml.Unmarshal(buf, &newFakeRules); err != nil {
		return err
	}
	newRules := make([]*Rule, len(newFakeRules))
	newNamedRules := make(map[string]int)
	for i, rule := range newFakeRules {
		r := &Rule{}
		r.Name = rule.Name
		r.Description = rule.Description
		r.WantsAttribs = rule.WantsAttribs
		r.Matchers = make([]Matcher, len(rule.Matchers))
		r.MatchActions = make([]Action, len(rule.MatchActions))
		if r.Name != "" {
			conflict, ok := newNamedRules[r.Name]
			if ok {
				return fmt.Errorf("Rule %d (name %s) has the same name as Rule %d", i, r.Name, conflict)
			}
			newNamedRules[r.Name] = i
		}

		if len(rule.Matchers) == 0 {
			log.Printf("Rule %d (name: %s) has no Matchers, which means it matches everything.", i, r.Name)
			log.Printf("This is probably not what you want")
		}

		for i, m := range rule.Matchers {
			j, err := ResolveMatcher(m)
			if err != nil {
				log.Printf("Rule %d (name: %s) failed to compile Matchers", i, r.Name)
				log.Printf("%s", err)
				return err
			}
			r.Matchers[i] = j
		}

		if len(rule.MatchActions) == 0 {
			log.Printf("Rule %d (name: %s) has no MatchActions, which means it does nothing.", i, r.Name)
			log.Printf("This is probably not what you want")
		}

		for i, a := range rule.MatchActions {
			j, err := ResolveAction(a)
			if err != nil {
				log.Printf("Rule %d (name: %s) failed to compile MatchActions", i, r.Name)
				log.Printf("%s", err)

				return err
			}
			r.MatchActions[i] = j
		}
		newRules[i] = r
	}
	ruleLock.Lock()
	log.Printf("Loading rules from %s", src)
	for name, i := range newNamedRules {
		namedRules[name] = newRules[i]
	}
	rules = newRules
	ruleLock.Unlock()
	return nil
}
