package main

import (
	"encoding/json"
	"log"
)

// Rule represents a list of Actions that shoudl be taken if an Event matches the Rule or not.
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
	UnmatchActions []Action // List of Actions to run when the
	// Rule does not match the Event.
}

func (r *Rule) UnmarshalJSON(data []byte) error {
	log.Printf("Unmarshalling rule %s", string(data))
	type fakeRule struct {
		Name           string
		Description    string
		WantsAttribs   []string
		Matchers       []map[string]interface{}
		MatchActions   []map[string]interface{}
		UnmatchActions []map[string]interface{}
	}
	res := &fakeRule{}
	if err := json.Unmarshal(data, &res); err != nil {
		return err
	}
	r.Name = res.Name
	r.Description = res.Description
	r.WantsAttribs = res.WantsAttribs
	r.Matchers = make([]Matcher, len(res.Matchers))
	r.MatchActions = make([]Action, len(res.MatchActions))
	r.UnmatchActions = make([]Action, len(res.UnmatchActions))

	for i, m := range res.Matchers {
		j, err := resolveMatcher(m)
		if err != nil {
			return err
		}
		r.Matchers[i] = j
	}

	for i, a := range res.MatchActions {
		j, err := resolveAction(a)
		if err != nil {
			return err
		}
		r.MatchActions[i] = j
	}
	for i, a := range res.UnmatchActions {
		j, err := resolveAction(a)
		if err != nil {
			return err
		}
		r.UnmatchActions[i] = j
	}
	return nil
}

// Fire runs the Matchers against the Event, and runs either the
// MatchActions or the UnmatchActions depending on whether the
// Matchers matched.  If the rule failed to Fire, either matcherr or
// runerr will container the error depending on whether there was an
// error determining whether the rule failed to match the event or
// there was an error running the relevant actions that should have
// been run.
func (r *Rule) Fire(e *Event) (matched bool, matcherr error, runerr error) {
	matched, matcherr = matchAnd(r.Matchers...)(e)
	if matcherr != nil {
		return
	}
	var actions []Action
	if matched {
		actions = r.MatchActions
	} else {
		actions = r.UnmatchActions
	}
	for _, action := range actions {
		runerr = action(e)
		if runerr != nil {
			return
		}
	}
	return
}
