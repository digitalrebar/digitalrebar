package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"log"

	"github.com/VictorLowther/jsonpatch/utils"
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
	fn, err := ResolveMatcher(res)
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

	type selector struct {
		Selector    string
		SaveAs      string
		PickResults map[string]float64
	}

	s := &selector{}
	if err := utils.Remarshal(val, &s); err != nil {
		return nil, err
	}

	if s.Selector == "" {
		return nil, errors.New("JSON requires a Selector")
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
		res, err := parser.GetValues(s.Selector)
		if err != nil {
			return false, err
		}
		if len(res) == 0 {
			return false, nil
		}
		log.Printf("JSON selector matched: %v", res)
		if len(s.PickResults) > 0 {
			for varToSave, fidx := range s.PickResults {
				idx := int(fidx)
				if idx >= len(res) {
					return false, fmt.Errorf("Cannot save variable %s, requested index %d out of bounds: %v", varToSave, idx, res)
				}
				e.Vars[varToSave] = res[idx]
			}
		}
		if s.SaveAs != "" {
			if len(res) == 1 {
				e.Vars[s.SaveAs] = res[0]
			} else {
				e.Vars[s.SaveAs] = res
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
	tmpl, err := compileScript(script)
	if err != nil {
		return nil, err
	}
	return func(e *RunContext) (bool, error) {
		return runScript(e, tmpl)
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
		j, err := ResolveMatcher(realV)
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

// ResolveMatcher compiles a map[string]interface{} with a single key-value pair
// into a function with a Matcher signature.
//
// Recognized keys are:
//
// "And" and "Or", which expect their values to be arrays of key-value pairs that
// can be compiled by ResolveMatch.  "And" will match if all of its matchers match,
// and "Or" will match if any of its matchers match.  Both operators will only execute
// enough of their matchers to determine if the overall matcher will succeed (for "Or"),
// or fail (for "And").  If no matchers are passed, "And" will signal that it matched,
// and "Or" will signal that it did not match.
//
// "Not", which expects its value to be a single key-value pair that can be compiled by
// ResolveMatcher.  "Not" will invert the return value of its matcher.
//
// "Enabled", which expects its value to be a bool.  It will return the value of that
// boolean without change, and (as the name suggests) is expected to enable and disable
// rules for testing purposes.
//
// "JSON", which expects a struct with the following format:
//   struct {
//		Selector    string
//		SaveAs      string
//		PickResults map[string]float64
//	 }
// "Selector" is a string containing a JSON Selector matching the specification at http://jsonselect.org/#overview.
// The selector will return an array of results containing each individual item from the RunContext that was passed into
// the Matcher.
// "SaveAs" is an optional variable name to save the results the selector returned.  If the selector returned exactly one item,
// then just that item will be saved, otherwise the entire array of results will be saved.  You should craft
// your selectors with that in mind.
// "PickResults" is an optional map of variables names to selector indexes.  After the selector returns its results and they
// have been optionally saved to the variable pointed to by SaveAs, PickResults will save data from the selector indexes to the
// matching variable names.  If the selector results do not contain a requested index, then the match will fail with an error.
// You should construct Selector and PickResults with that in mind.
// In the absence of PickResults, a JSON match fails if nothing was selected or the Selector was invalid, otherwise it passes.
//
// "Script", which expects its value to be a string that will be parsed using text.Template.  The result of that parsing should
// be a valid bash script.  When the Matcher is ran, the parsed script will be compiled using the passed RunContext.  If the compilation fails,
// the match will fail with an error, so be sure to write your scripts with that in mind.  Otherwise, the matcher will pass if the script
// exits with a zero and fail otherwise.
func ResolveMatcher(m map[string]interface{}) (Matcher, error) {
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
