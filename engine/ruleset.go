package engine

import (
	"fmt"
	"log"

	"github.com/pborman/uuid"
)

// Rule defines a set of Actions to perform if the Matchers match.
type Rule struct {
	// Name of the Rule.  Not required, but unnnamed Rules can only
	// be invoked by direct triggers from an incoming Event.
	Name string
	// Events that will trigger this Rule. A Rule can be
	// triggered by multiple Events, and multiple Rules in
	// a RuleSet can be triggered by the same Event. In
	// that case, the Rules will be run in the order in
	// which they were defined.
	EventSelectors []EventSelector
	// The Attributes that the rule needs to perform its job.
	WantsAttribs []string
	// The matchers must all match for the Actions to be performed.
	// If any of the matchers do not match, the Actions will not be performed
	// and processing will proceed to the next Step in the Rule.
	Matchers []map[string]interface{}
	// If all the Matchers match, the Actions for the Step will be performed.
	// If any of the Actions fail, then the Rule fails overall.
	Actions  []map[string]interface{}
	matchers []matcher // compiled Matchers
	actions  []action  // compiled Actions
}

// Match runs the step Matchers.  If all the Matchers match, then the
// step is considered an overall Match.
func (r *Rule) match(c *RunContext) (bool, error) {
	return matchAnd(r.matchers...)(c)
}

// RunActions runs the step MatchActions in order.  If any of them
// return an error, no further MatchActions will be run.
func (r *Rule) run(c *RunContext) error {
	for _, action := range r.actions {
		if err := action(c); err != nil {
			return err
		}
	}
	return nil
}

// RuleSet defines a discrete bundle of functionality
// that the Classifier operates on. Multiple Rulesets can
// be loaded into the Classifier, and they will all be processed independently.
type RuleSet struct {
	// Rulesets that are not Active will not recieve Events, and therefore
	// will not do anything.
	Active bool
	// Globally unique name of the RuleSet
	Name string
	// Human-readable description of what this RuleSet is for.
	Description string
	// Version of the RuleSet.  Used to detect conflicting RuleSet changes.
	// API calls must provide this on Update and Delete calls.
	Version uuid.UUID
	// List of Rules that this RuleSet encapsulates.  The order of the Rules
	// is important, because Call and Jump actions can only call named Rules
	// that appear earlier in the Rule list.  This is important because we
	// need to guarantee that the processing of any given Event will eventually
	// terminate, and one of the better ways to do that is to ensure that we cannot
	// have loops in rule processing.  This restriction may be relaxed in the future.
	Rules      []Rule
	engine     *Engine
	namedRules map[string]int
}

func (rs *RuleSet) compile(e *Engine) error {
	for i := range rs.Rules {
		rule := &rs.Rules[i]
		// Make sure we don't have conflicting rule names
		if rule.Name != "" {
			conflict, ok := rs.namedRules[rule.Name]
			if ok {
				return fmt.Errorf("Ruleset %s: Rule %d (name %s) has the same name as Rule %d", rs.Name, i, rule.Name, conflict)
			}
			log.Printf("Adding named rule '%s'", rule.Name)
			rs.namedRules[rule.Name] = i
		}
	}
	for i := range rs.Rules {
		rule := &rs.Rules[i]
		rule.matchers = make([]matcher, len(rule.Matchers))
		rule.actions = make([]action, len(rule.Actions))
		for l, m := range rule.Matchers {
			match, err := resolveMatcher(rule, m)
			if err != nil {
				return err
			}
			rule.matchers[l] = match
		}
		for l, a := range rule.Actions {
			action, err := resolveAction(rs, i, a)
			if err != nil {
				return err
			}
			rule.actions[l] = action
		}
	}
	return nil
}

type etInvoker struct {
	selector    EventSelector
	ruleIndexes map[string][]int
}
