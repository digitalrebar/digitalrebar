package main

import (
	"encoding/json"
	"fmt"
	"log"
	"os/exec"
	"strings"
)

var matchTypes = []string{"And", "Or", "Not", "Script", "Enabled"}
var actionTypes = []string{"Log"}

type Matcher func(*Event) (bool, error)

func matchAnd(funcs ...Matcher) Matcher {
	return func(e *Event) (bool, error) {
		for _, fn := range funcs {
			ok, err := fn(e)
			if err != nil {
				return false, err
			} else if !ok {
				return false, nil
			}
		}
		return true, nil
	}
}

func matchOr(funcs ...Matcher) Matcher {
	return func(e *Event) (bool, error) {
		for _, fn := range funcs {
			ok, err := fn(e)
			if err != nil {
				return false, err
			} else if ok {
				return true, nil
			}
		}
		return false, nil
	}
}

func matchNot(fn Matcher) Matcher {
	return func(e *Event) (bool, error) {
		ok, err := fn(e)
		return !ok, err
	}
}

func matchBool(b bool) Matcher {
	return func(e *Event) (bool, error) {
		return b, nil
	}
}

func matchScript(script string) Matcher {
	return func(e *Event) (bool, error) {
		cmd := exec.Command("/usr/bin/env", "bash", "-x")
		cmd.Stdin = strings.NewReader(fmt.Sprintf("CLASSIFIER_ATTRIBS='%s'\n%s",
			e.attribsJSON(),
			script))
		out, err := cmd.Output()
		if err == nil {
			log.Printf("Script rule %s ran successfully", e.rule.Name)
			log.Printf("%s", string(out))
			return true, nil
		} else {
			log.Printf("Script rule %s failed", e.rule.Name)
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
}

type Action func(*Event) error

func actionLog() Action {
	return func(e *Event) error {
		log.Printf("Event %s matched rule %s for node %s",
			e.Name,
			e.rule.Name,
			e.Node.Name)
		return nil
	}
}

func resolveMatchArray(op string, matchers []Matcher) Matcher {
	switch op {
	case "And":
		return matchAnd(matchers...)
	case "Or":
		return matchOr(matchers...)
	default:
		log.Panicf("%s was op to resolveMatchArray, cannot happen")
		return nil
	}
}

func resolveMatcher(m map[string]interface{}) (Matcher, error) {
	if len(m) != 1 {
		return nil, fmt.Errorf("Matchers have exactly one key")
	}
	for _, t := range matchTypes {
		if _, ok := m[t]; !ok {
			continue
		}
		switch t {
		case "And", "Or":
			vals, ok := m[t].([]interface{})
			if !ok {
				return nil, fmt.Errorf("%s needs an Array", t)
			}
			res := make([]Matcher, len(vals))
			for i, v := range vals {
				realV, ok := v.(map[string]interface{})
				if !ok {
					return nil, fmt.Errorf("%s member %v must be a map", t, i)
				}
				j, err := resolveMatcher(realV)
				if err != nil {
					return nil, err
				}
				res[i] = j
			}
			return resolveMatchArray(t, res), nil
		case "Not":
			val, ok := m[t].(map[string]interface{})
			if !ok {
				return nil, fmt.Errorf("%s needs a map", t)
			}
			j, err := resolveMatcher(val)
			if err != nil {
				return nil, err
			}
			return matchNot(j), nil
		case "Script":
			j, ok := m[t].(string)
			if !ok {
				return nil, fmt.Errorf("%s needs a string", t)
			}
			return matchScript(j), nil
		case "Enabled":
			j, ok := m[t].(bool)
			if !ok {
				return nil, fmt.Errorf("%s needs a bool", t)
			}
			return matchBool(j), nil
		default:
			return nil, fmt.Errorf("Unknown matcher %s", t)
		}
	}
	return nil, fmt.Errorf("Cannot unmarshal matcher for #%v", m)
}

func resolveAction(a map[string]interface{}) (Action, error) {
	if len(a) != 1 {
		return nil, fmt.Errorf("Actions have exactly one key")
	}
	for _, t := range actionTypes {
		if _, ok := a[t]; !ok {
			continue
		}
		switch t {
		case "Log":
			return actionLog(), nil
		default:
			return nil, fmt.Errorf("Unknown action %s", t)
		}
	}
	return nil, fmt.Errorf("Cannot unmarshal action for #%v", a)
}

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
