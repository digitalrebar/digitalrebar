package main

import (
	"encoding/json"
	"log"
)

type Rule struct {
	Name         string
	Description  string
	WantsAttribs []string
	Matchers     []Matcher
	Actions      []Action
}

func (r *Rule) UnmarshalJSON(data []byte) error {
	log.Printf("Unmarshalling rule %s", string(data))
	type fakeRule struct {
		Name         string
		Description  string
		WantsAttribs []string
		Matchers     []map[string]interface{}
		Actions      []map[string]interface{}
	}
	res := &fakeRule{}
	if err := json.Unmarshal(data, &res); err != nil {
		return err
	}
	r.Name = res.Name
	r.Description = res.Description
	r.WantsAttribs = res.WantsAttribs
	r.Matchers = make([]Matcher, len(res.Matchers))
	r.Actions = make([]Action, len(res.Actions))

	for i, m := range res.Matchers {
		j, err := resolveMatcher(m)
		if err != nil {
			return err
		}
		r.Matchers[i] = j
	}

	for i, a := range res.Actions {
		j, err := resolveAction(a)
		if err != nil {
			return err
		}
		r.Actions[i] = j
	}
	return nil
}

func (r *Rule) Match(e *Event) (bool, error) {
	return matchAnd(r.Matchers...)(e)
}

func (r *Rule) Run(e *Event) error {
	for _, action := range r.Actions {
		if err := action(e); err != nil {
			return err
		}
	}
	return nil
}
