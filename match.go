package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"log"
	"reflect"
	"strings"

	"github.com/VictorLowther/jsonpatch/utils"
	js "github.com/coddingtonbear/go-jsonselect"
	"github.com/digitalrebar/rebar-api/client"
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

func matchNot(r *Rule, val interface{}) (Matcher, error) {
	res, ok := val.(map[string]interface{})
	if !ok {
		return nil, errors.New("Not needs a map")
	}
	fn, err := ResolveMatcher(r, res)
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

type JSONselector struct {
	Selector    string
	SaveAs      string
	PickResults map[string]float64
}

func matchJSON(val interface{}) (Matcher, error) {

	s := &JSONselector{}
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
		if debug {
			log.Printf("Matching JSON selector %s to:\n%s", s.Selector, jsonOut)
		}
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
		if s.SaveAs != "" {
			e.Vars[s.SaveAs] = res
		}
		if len(s.PickResults) > 0 {
			for varToSave, fidx := range s.PickResults {
				idx := int(fidx)
				if idx >= len(res) {
					return false, fmt.Errorf("Cannot save variable %s, requested index %d out of bounds: %v", varToSave, idx, res)
				}
				e.Vars[varToSave] = res[idx]
			}
		}

		return true, nil
	}, nil
}

const (
	badType = iota
	boolType
	intType
	uintType
	floatType
	stringType
)

func baseType(t reflect.Value) int {
	switch t.Kind() {
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return intType
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		return uintType
	case reflect.Float32, reflect.Float64:
		return floatType
	case reflect.String:
		return stringType
	default:
		return badType
	}

}

func canOrder(a, b int) bool {
	if a == boolType || a == badType || b == boolType || b == badType {
		return false
	}
	if a == b {
		return true
	}
	if a == stringType || b == stringType {
		return false
	}
	if a == floatType || b == floatType {
		return false
	}
	return true
}

func lt(arg1, arg2 interface{}) (bool, error) {
	a := reflect.ValueOf(arg1)
	b := reflect.ValueOf(arg2)
	aType := baseType(a)
	bType := baseType(b)
	if !canOrder(aType, bType) {
		return false, fmt.Errorf("Types %s(%v) and %s(%v) do not have an ordering comparison", a.Kind(), arg1, b.Kind(), arg2)
	}
	switch aType {
	case stringType:
		return a.String() < b.String(), nil
	case floatType:
		return a.Float() < b.Float(), nil
	case intType:
		switch bType {
		case intType:
			return a.Int() < b.Int(), nil
		case uintType:
			return uint64(a.Int()) < b.Uint(), nil
		}
	case uintType:
		switch bType {
		case uintType:
			return a.Uint() < b.Uint(), nil
		case intType:
			return a.Uint() < uint64(b.Int()), nil
		}
	}
	log.Panicf("Tried to order %s and %s, reached impossible condition", a.Kind(), b.Kind())
	return false, nil
}

func matchLen(val interface{}) (Matcher, error) {
	type lenArg struct {
		Var    string
		SaveAs string
	}
	arg := &lenArg{}
	if err := utils.Remarshal(val, &arg); err != nil {
		return nil, err
	}
	if arg.Var == "" {
		return nil, errors.New("Len requires a variable name")
	}
	if arg.SaveAs == "" {
		return nil, errors.New("Len requires a variable to save the length into")
	}
	return func(e *RunContext) (bool, error) {
		val, ok := e.Vars[arg.Var]
		if !ok {
			return false, fmt.Errorf("Len: variable %s not set", arg.Var)
		}
		reflectVal := reflect.ValueOf(val)
		if !reflectVal.IsValid() {
			log.Panicf("Var %s was not valid! This should never happen", arg.Var)
		}
		switch reflectVal.Kind() {
		case reflect.Map, reflect.Slice, reflect.String:
			e.Vars[arg.SaveAs] = float64(reflectVal.Len())
			return true, nil
		default:
			return false, nil
		}
	}, nil

}

func getAttrib(val interface{}) (Matcher, error) {
	thing, ok := val.(map[string]interface{})
	if !ok {
		return nil, fmt.Errorf("GetAttrib: Expeected a map[string]interface{}, got #%v", val)
	}
	var tgt, attr, id, saveAs string
	for k, fv := range thing {
		v, ok := fv.(string)
		if !ok {
			fmt.Errorf("GetAttrib: values must be strings, not %#v", fv)
		}
		switch k {
		case "Node", "Role", "Deployment", "NodeRole", "DeploymentRole":
			if tgt != "" {
				return nil, fmt.Errorf("GetAttrib: Already looking for %s, cannot also look for %s", tgt, k)
			}
			tgt = k
			id = v
		case "Attrib":
			if attr != "" {
				return nil, fmt.Errorf("GetAttrib: Already getting attrib %s, cannot also look for %s", attr, v)
			}
			attr = v
		case "SaveAs":
			if saveAs != "" {
				return nil, fmt.Errorf("GetAttrib: Already saving attr to %s, cannot also save it to %s", saveAs, v)
			}
			saveAs = v
		default:
			return nil, fmt.Errorf("GetAttrib: Unknown key %s", k)
		}
	}
	return func(c *RunContext) (bool, error) {
		var attrSrc client.Attriber
		switch tgt {
		case "Node":
			attrSrc = &client.Node{}
		case "Role":
			attrSrc = &client.Role{}
		case "Deployment":
			attrSrc = &client.Deployment{}
		case "NodeRole":
			attrSrc = &client.NodeRole{}
		case "DeploymentRole":
			attrSrc = &client.DeploymentRole{}
		default:
			log.Panicf("GetAttrib: lookup of %s cannot happen!", tgt)
		}
		if err := client.Fetch(attrSrc, id); err != nil {
			return false, err
		}
		attrVal, err := client.FetchAttrib(attrSrc, attr, "")
		if err != nil {
			return false, err
		}
		c.Vars[saveAs] = attrVal.Value
		return true, nil
	}, nil
}

func getThingUUID(val interface{}) (Matcher, error) {
	thing, ok := val.(map[string]interface{})
	if !ok {
		return nil, fmt.Errorf("Expected a map[string]interface{}")
	}
	if len(thing) != 2 {
		return nil, fmt.Errorf("Get: Expected one thing to get")
	}
	var tgt, id, saveAs string
	for k, fv := range thing {
		v, ok := fv.(string)
		if !ok {
			return nil, fmt.Errorf("Get: values must be strings!")
		}
		switch k {
		case "Node", "Role", "NodeRole", "Deployment", "DeploymentRole":
			if tgt != "" {
				return nil, fmt.Errorf("Get: already looking for %s, will not get %s", tgt, k)
			}
			tgt = k
			id = v
		case "SaveAs":
			if saveAs != "" {
				return nil, fmt.Errorf("Get: already saving as %s, refusing to save as %s", saveAs, v)
			}
			saveAs = v
		default:
			return nil, fmt.Errorf("Don't know how to get %s", id)
		}
	}
	return func(c *RunContext) (bool, error) {
		v, err := c.getVar(id)
		if err != nil {
			return false, err
		}
		id, ok := v.(string)
		if !ok {
			return false, fmt.Errorf("Get: %#v is not a string", v)
		}
		var uuid string
		switch tgt {
		case "Node":
			obj := &client.Node{}
			if err := client.Fetch(obj, id); err != nil {
				return false, err
			}
			uuid = obj.UUID
		case "Role":
			obj := &client.Role{}
			if err := client.Fetch(obj, id); err != nil {
				return false, err
			}
			uuid = obj.UUID
		case "Deployment":
			obj := &client.Deployment{}
			if err := client.Fetch(obj, id); err != nil {
				return false, err
			}
			uuid = obj.UUID
		case "NodeRole":
			obj := &client.NodeRole{}
			if err := client.Fetch(obj, id); err != nil {
				return false, err
			}
			uuid = obj.UUID
		case "DeploymentRole":
			obj := &client.DeploymentRole{}
			if err := client.Fetch(obj, id); err != nil {
				return false, err
			}
			uuid = obj.UUID
		default:
			log.Panicf("Get: looking for %s, cannot happen!", tgt)
		}
		c.Vars[saveAs] = uuid
		return true, nil
	}, nil
}

func matchCmp(op string, val interface{}) (Matcher, error) {
	fixedArgs, ok := val.([]interface{})
	if !ok || len(fixedArgs) != 2 {
		return nil, fmt.Errorf("%s: Expected a 2 element array", op)
	}

	for _, arg := range fixedArgs {
		if varRef, ok := arg.(string); ok && strings.HasPrefix(varRef, `$`) {
			continue
		}
		switch op {
		// Eq and Ne use reflect.DeepEqual, which can
		// equality-check anything JSON compatible.
		case "Lt", "Le", "Gt", "Ge":
			if baseType(reflect.ValueOf(arg)) == badType {
				// This is a comparison op that relies
				// on ordering, but the type is not
				// orderable.
				return nil, fmt.Errorf("%s: Value argument %v is not an orderable type", op, arg)
			}
		}
	}
	return func(e *RunContext) (bool, error) {
		a, err := e.getVar(fixedArgs)
		if err != nil {
			return false, fmt.Errorf("Failed to fetch variable for %s on %v: %v", op, val, err)
		}
		args := a.([]interface{})
		log.Printf("Comparing %v %s %v", args[0], op, args[1])
		switch op {
		case "Eq":
			return reflect.DeepEqual(args[0], args[1]), nil
		case "Ne":
			return !reflect.DeepEqual(args[0], args[1]), nil
		case "Lt":
			return lt(args[0], args[1])
		case "Gt":
			return lt(args[1], args[0])
		case "Le":
			res, err := lt(args[0], args[1])
			if !res && err == nil {
				res = reflect.DeepEqual(args[0], args[1])
			}
			return res, err
		case "Ge":
			res, err := lt(args[1], args[0])
			if !res && err == nil {
				res = reflect.DeepEqual(args[0], args[1])
			}
			return res, err
		default:
			log.Panicf("Op %s not implemented! Should never reach here", op)
		}
		return false, nil

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

func resolveAndOr(r *Rule, op string, val interface{}) (Matcher, error) {
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
		j, err := ResolveMatcher(r, realV)
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

func matchEventType(r *Rule, val interface{}) (Matcher, error) {
	eventList, ok := val.([]map[string]interface{})
	if !ok {
		eventList2, ok := val.([]interface{})
		if !ok {
			return nil, fmt.Errorf("EventType needs an array of events: %v", val)
		}
		eventList = make([]map[string]interface{}, len(eventList2), len(eventList2))
		for i, e := range eventList2 {
			re, ok := e.(map[string]interface{})
			if !ok {
				return nil, errors.New("EventType needs an event as a key/value map")
			}
			eventList[i] = re
		}
	}

	eventListJson, err := json.Marshal(eventList)
	if err != nil {
		return nil, err
	}
	eventListJsonString := string(eventListJson)

	r.EventSelectors = eventList

	return func(e *RunContext) (bool, error) {
		selector := e.Evt.Selector
		if debug {
			log.Printf("Matching EventType selector %v to:\n%s", selector, eventListJsonString)
		}
		parser, err := js.CreateParserFromString(eventListJsonString)
		if err != nil {
			return false, err
		}

		selectorString := ""
		first := true
		for k, v := range selector {
			if !first {
				selectorString = selectorString + " object"
			}
			selectorString = selectorString + ":has(." + k + ":val(\"" + v + "\"))"
			first = false
		}

		res, err := parser.GetValues(selectorString)
		if err != nil {
			return false, err
		}
		if len(res) == 0 {
			return false, nil
		}
		log.Printf("EventType selector matched: %v", res)
		e.Vars["eventType"] = res[0]

		return true, nil
	}, nil
}

// ResolveMatcher compiles a map[string]interface{} with a single
// key-value pair into a function with a Matcher signature.
//
// Recognized keys are:
//
// "And" and "Or", which expect their values to be arrays of key-value
// pairs that can be compiled by ResolveMatch.  "And" will match if
// all of its matchers match, and "Or" will match if any of its
// matchers match.  Both operators will only execute enough of their
// matchers to determine if the overall matcher will succeed (for
// "Or"), or fail (for "And").  If no matchers are passed, "And" will
// signal that it matched, and "Or" will signal that it did not match.
//
// "Not", which expects its value to be a single key-value pair that
// can be compiled by ResolveMatcher.  "Not" will invert the return
// value of its matcher.
//
// "Enabled", which expects its value to be a bool.  It will return
// the value of that boolean without change, and (as the name
// suggests) is expected to enable and disable rules for testing
// purposes.
//
// "JSON", which expects a struct with the following format:
//   struct {
//		Selector    string
//		SaveAs      string
//		PickResults map[string]float64
//	 }
//
// "Selector" is a string containing a JSON Selector matching the
// specification at http://jsonselect.org/#overview.  The selector
// will return an array of results containing each individual item
// from the RunContext that was passed into the Matcher.
//
// "SaveAs" is an optional variable name to save the results the
// selector returned.  The entire array of results will be saved, even
// if it is only one element long.
//
// "PickResults" is an optional map of variables names to selector
// indexes.  After the selector returns its results and they have been
// optionally saved to the variable pointed to by SaveAs, PickResults
// will save data from the selector indexes to the matching variable
// names.  If the selector results do not contain a requested index,
// then the match will fail with an error.  You should construct
// Selector and PickResults with that in mind.  In the absence of
// PickResults, a JSON match fails if nothing was selected or the
// Selector was invalid, otherwise it passes.
//
// "Script", which expects its value to be a string that will be
// parsed using text.Template.  The result of that parsing should be a
// valid bash script.  When the Matcher is ran, the parsed script will
// be compiled using the passed RunContext.  If the compilation fails,
// the match will fail with an error, so be sure to write your scripts
// with that in mind.  Otherwise, the matcher will pass if the script
// exits with a zero and fail otherwise.
//
// "Eq", "Ne", "Lt", "Le", "Gt", "Ge", which are basic 2-item
// comparison functions that expect a 2 element array of values.  If
// the value is a string that begins with '$', the string will be
// interpreted as the name of a variable.  If that variable has been
// set, its value will be substituted.  To use a literal string
// beginning with $, escape it with a \.  To begin with a literal \,
// escape it with \\.
//
// "Len", which expects its value to be a struct with the following
// format:
//
//    struct {
//        Var string
//        SaveAs string
//    }
//
// If the variable saved in Var is something with a length (an array,
// a map, or a string), the matcher will return true and save the
// length of the variable in the variable named by SaveAs, otherwise
// it will return false.
//
// "UUID", which expects its value to be a struct matching the
// following format:
//
//    struct {
//        Node string
//        Role string
//        Deployment string
//        NodeRole string
//        DeploymentRole string
//        SaveAs string
//    }
//
// Out if those fields, exactly one of Node, Role, Deployment,
// NodeRole, and DeploymentRole should be filled, and their values must
// resolve to an existing object of their type, otherwise the matcher will not match.
// If there is such an object, its UUID will be saved in SaveAs.
//
// "GetAttrib", which expects its value to be a struct wuth the following format:
//
//    struct {
//        Attrib string
//        Node string
//        Role string
//        Deployment string
//        NodeRole string
//        DeploymentRole string
//        SaveAs string
//    }
//
//
// Out if those fields, exactly one of Node, Role, Deployment,
// NodeRole, and DeploymentRole should be filled, and their values
// must resolve to an existing object of their type, otherwise the
// matcher will not match.  Attrib must also be filled with the name
// of the Attrib to fetch.  If the vlue of the attrib can be fetched
// for the object, it will be saved in the variable named by SaveAs
//
// EventType, which expects its value to be a string or list of strings that
// specify the list of events this rule should trigger for.
func ResolveMatcher(r *Rule, m map[string]interface{}) (Matcher, error) {
	if len(m) != 1 {
		return nil, fmt.Errorf("Matchers have exactly one key")
	}
	for t, v := range m {
		switch t {
		case "And", "Or":
			return resolveAndOr(r, t, v)
		case "Not":
			return matchNot(r, v)
		case "Script":
			return matchScript(v)
		case "Enabled":
			return matchEnabled(v)
		case "JSON":
			return matchJSON(v)
		case "Eq", "Ne", "Lt", "Le", "Gt", "Ge":
			return matchCmp(t, v)
		case "Len":
			return matchLen(v)
		case "UUID":
			return getThingUUID(v)
		case "GetAttrib":
			return getAttrib(v)
		case "EventType":
			return matchEventType(r, v)
		default:
			return nil, fmt.Errorf("Unknown matcher %s", t)
		}
	}
	return nil, fmt.Errorf("Cannot unmarshal matcher for %#v", m)
}
