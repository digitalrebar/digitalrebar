package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"log"

	js "github.com/coddingtonbear/go-jsonselect"
)

// Matcher is what is used by Rules to determine whether they shouuld
// fire for a given Event.  Right now, we only have the basic
// combinators, a simple static boolean matcher, and a matcher that
// runs a script and uses the exit value to determine whether it
// matched or not.
type Matcher func(*RunContext) (bool, error)

func matchAnd(funcs ...Matcher) Matcher {
	return func(e *RunContext) (bool, error) {
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
	return func(e *RunContext) (bool, error) {
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

func matchNot(val interface{}) (Matcher, error) {
	res, ok := val.(map[string]interface{})
	if !ok {
		return nil, errors.New("Not needs a map")
	}
	fn, err := resolveMatcher(res)
	if err != nil {
		return nil, err
	}
	return func(e *RunContext) (bool, error) {
		ok, err := fn(e)
		return !ok, err
	}, nil
}

func matchEnabled(val interface{}) (Matcher, error) {
	b, ok := val.(bool)
	if !ok {
		return nil, errors.New("Enabled needs a bool")
	}
	return func(e *RunContext) (bool, error) {
		return b, nil
	}, nil
}

func matchJSON(val interface{}) (Matcher, error) {
	selectorFrag, ok := val.(map[string]interface{})
	if !ok {
		return nil, errors.New("JSON needs an object")
	}
	if _, ok := selectorFrag["Selector"]; !ok {
		return nil, errors.New("JSON requires a Selector")
	}
	selector, ok := selectorFrag["Selector"].(string)
	if !ok {
		return nil, errors.New("JSON.Selector must be a string")
	}
	resultsMap := map[string]int{}
	var saveAs string
	if _, ok := selectorFrag["Results"]; ok {
		// We want to map results back into variables.
		resultsMapFrag, ok := selectorFrag["PickResults"].(map[string]interface{})
		if !ok {
			return nil, errors.New("JSON.PickResults must be an object")
		}
		for varName, index := range resultsMapFrag {
			idx, ok := index.(float64)
			if !ok {
				return nil, fmt.Errorf("JSON.PickResults[%s] must be numeric", varName)
			}
			resultsMap[varName] = int(idx)
		}
	}
	saveAsFrag, ok := selectorFrag["SaveAs"]
	if ok {
		saveAs, ok = saveAsFrag.(string)
		if !ok {
			return nil, errors.New("JSON.SaveAs must be a string")
		}
	}
	return func(e *RunContext) (bool, error) {
		buf, err := json.Marshal(e)
		if err != nil {
			return false, err
		}
		jsonOut := string(buf)
		parser, err := js.CreateParserFromString(jsonOut)
		if err != nil {
			return false, err
		}
		res, err := parser.GetValues(selector)
		if err != nil {
			return false, err
		}
		if len(res) == 0 {
			return false, nil
		}
		log.Printf("JSON selector matched: %v", res)
		if len(resultsMap) > 0 {
			for varToSave, idx := range resultsMap {
				if idx >= len(res) {
					return false, fmt.Errorf("Cannot save variable %s, requested index %d out of bounds: %v", varToSave, idx, res)
				}
				e.Vars[varToSave] = res[idx]
			}
		}
		if saveAs != "" {
			if len(res) == 1 {
				e.Vars[saveAs] = res[0]
			} else {
				e.Vars[saveAs] = res
			}
		}
		return true, nil
	}, nil
}

func matchScript(val interface{}) (Matcher, error) {
	script, ok := val.(string)
	if !ok {
		return nil, errors.New("Script needs a string")
	}
	return func(e *RunContext) (bool, error) {
		return runScript(e, script)
	}, nil
}

func resolveAndOr(op string, val interface{}) (Matcher, error) {
	vals, ok := val.([]interface{})
	if !ok {
		return nil, fmt.Errorf("%s needs an Array", op)
	}
	res := make([]Matcher, len(vals))
	for i, v := range vals {
		realV, ok := v.(map[string]interface{})
		if !ok {
			return nil, fmt.Errorf("%s member %v must be a map", op, i)
		}
		j, err := resolveMatcher(realV)
		if err != nil {
			return nil, err
		}
		res[i] = j
	}
	switch op {
	case "And":
		return matchAnd(res...), nil
	case "Or":
		return matchOr(res...), nil
	default:
		log.Panicf("%s was op to resolveMatchArray, cannot happen", op)
		return nil, nil
	}
}

func resolveMatcher(m map[string]interface{}) (Matcher, error) {
	if len(m) != 1 {
		return nil, fmt.Errorf("Matchers have exactly one key")
	}
	for t, v := range m {
		switch t {
		case "And", "Or":
			return resolveAndOr(t, v)
		case "Not":
			return matchNot(v)
		case "Script":
			return matchScript(v)
		case "Enabled":
			return matchEnabled(v)
		case "JSON":
			return matchJSON(v)
		default:
			return nil, fmt.Errorf("Unknown matcher %s", t)
		}
	}
	return nil, fmt.Errorf("Cannot unmarshal matcher for %#v", m)
}
