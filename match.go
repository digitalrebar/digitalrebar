package main

import (
	"fmt"
	"log"
)

var matchTypes = []string{"And", "Or", "Not", "Script", "Enabled"}

// Matcher is what is used by Rules to determine whether they shouuld
// fire for a given Event.  Right now, we only have the basic
// combinators, a simple static boolean matcher, and a matcher that
// runs a script and uses the exit value to determine whether it
// matched or not.
type Matcher func(*runContext) (bool, error)

func matchAnd(funcs ...Matcher) Matcher {
	return func(e *runContext) (bool, error) {
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
	return func(e *runContext) (bool, error) {
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
	return func(e *runContext) (bool, error) {
		ok, err := fn(e)
		return !ok, err
	}
}

func matchBool(b bool) Matcher {
	return func(e *runContext) (bool, error) {
		return b, nil
	}
}

func matchScript(script string) Matcher {
	return func(e *runContext) (bool, error) {
		return runScript(e, script)
	}
}

func resolveMatchArray(op string, matchers []Matcher) Matcher {
	switch op {
	case "And":
		return matchAnd(matchers...)
	case "Or":
		return matchOr(matchers...)
	default:
		log.Panicf("%s was op to resolveMatchArray, cannot happen", op)
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
