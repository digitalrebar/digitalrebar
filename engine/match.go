package engine

/*
Copyright (c) 2016, Rackn Inc.
Licensed under the terms of the Digital Rebar License.
See LICENSE.md at the top of this repository for more information.
*/

import (
	"encoding/json"
	"errors"
	"fmt"
	"log"
	"reflect"
	"strings"

	"github.com/VictorLowther/jsonpatch/utils"
	js "github.com/coddingtonbear/go-jsonselect"
	"github.com/digitalrebar/rebar-api/api"
)

// Matcher is what is used by Rules to determine whether they shouuld
// fire for a given Event.  Right now, we only have the basic
// combinators, a simple static boolean matcher, and a matcher that
// matched or not.
type matcher func(*RunContext) (bool, error)

func matchAnd(funcs ...matcher) matcher {
	return func(c *RunContext) (bool, error) {
		for _, fn := range funcs {
			ok, err := fn(c)
			if err != nil {
				return false, err
			} else if !ok {
				return false, nil
			}
		}
		return true, nil
	}
}

func matchOr(funcs ...matcher) matcher {
	return func(c *RunContext) (bool, error) {
		for _, fn := range funcs {
			ok, err := fn(c)
			if err != nil {
				return false, err
			} else if ok {
				return true, nil
			}
		}
		return false, nil
	}
}

func matchNot(e *Engine, r *Rule, val interface{}) (matcher, error) {
	res, ok := val.(map[string]interface{})
	if !ok {
		return nil, errors.New("Not needs a map")
	}
	fn, err := resolveMatcher(e, r, res)
	if err != nil {
		return nil, err
	}
	return func(c *RunContext) (bool, error) {
		ok, err := fn(c)
		return !ok, err
	}, nil
}

func matchEnabled(val interface{}) (matcher, error) {
	b, ok := val.(bool)
	if !ok {
		return nil, errors.New("Enabled needs a bool")
	}
	return func(c *RunContext) (bool, error) {
		return b, nil
	}, nil
}

type jsonSelector struct {
	Selector    string
	SaveAs      string
	PickResults map[string]float64
}

func matchJSON(val interface{}) (matcher, error) {

	s := &jsonSelector{}
	if err := utils.Remarshal(val, &s); err != nil {
		return nil, err
	}

	if s.Selector == "" {
		return nil, errors.New("JSON requires a Selector")
	}

	return func(c *RunContext) (bool, error) {
		buf, err := json.Marshal(c)
		if err != nil {
			return false, err
		}
		jsonOut := string(buf)
		if c.Engine.Debug {
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
			c.Vars[s.SaveAs] = res
		}
		if len(s.PickResults) > 0 {
			for varToSave, fidx := range s.PickResults {
				idx := int(fidx)
				if idx >= len(res) {
					return false, fmt.Errorf("Cannot save variable %s, requested index %d out of bounds: %v", varToSave, idx, res)
				}
				c.Vars[varToSave] = res[idx]
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

func matchLen(val interface{}) (matcher, error) {
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
	return func(c *RunContext) (bool, error) {
		val, ok := c.Vars[arg.Var]
		if !ok {
			return false, fmt.Errorf("Len: variable %s not set", arg.Var)
		}
		reflectVal := reflect.ValueOf(val)
		if !reflectVal.IsValid() {
			log.Panicf("Var %s was not valid! This should never happen", arg.Var)
		}
		switch reflectVal.Kind() {
		case reflect.Map, reflect.Slice, reflect.String:
			c.Vars[arg.SaveAs] = float64(reflectVal.Len())
			return true, nil
		default:
			return false, nil
		}
	}, nil

}

func getAttrib(val interface{}) (matcher, error) {
	thing, ok := val.(map[string]interface{})
	if !ok {
		return nil, fmt.Errorf("GetAttrib: Expeected a map[string]interface{}, got #%v", val)
	}
	var tgt, attr, id, saveAs string
	for k, fv := range thing {
		v, ok := fv.(string)
		if !ok {
			return nil, fmt.Errorf("GetAttrib: values must be strings, not %#v", fv)
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
		var attrSrc api.Attriber
		fixedId, err := c.getVar(id)
		if err != nil {
			return false, err
		}
		id, ok := fixedId.(string)
		if !ok {
			return false, fmt.Errorf("GetAttrib: thing id %#v does not resolve to a String", fixedId)
		}
		fixedAttr, err := c.getVar(attr)
		if err != nil {
			return false, err
		}
		attrID, ok := fixedAttr.(string)
		if !ok {
			return false, fmt.Errorf("GetAttrib: attrib name %#v does not resolve to a String", fixedAttr)
		}

		switch tgt {
		case "Node":
			attrSrc = &api.Node{}
		case "Role":
			attrSrc = &api.Role{}
		case "Deployment":
			attrSrc = &api.Deployment{}
		case "NodeRole":
			attrSrc = &api.NodeRole{}
		case "DeploymentRole":
			attrSrc = &api.DeploymentRole{}
		default:
			log.Panicf("GetAttrib: lookup of %s cannot happen!", tgt)
		}
		if err := c.Client.Fetch(attrSrc, id); err != nil {
			return false, err
		}
		attrVal, err := c.Client.FetchAttrib(attrSrc, attrID, "")
		if err != nil {
			return false, err
		}
		c.Vars[saveAs] = attrVal.Value
		return true, nil
	}, nil
}

func getThingUUID(val interface{}) (matcher, error) {
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
			obj := &api.Node{}
			if err := c.Client.Fetch(obj, id); err != nil {
				return false, err
			}
			uuid = obj.UUID
		case "Role":
			obj := &api.Role{}
			if err := c.Client.Fetch(obj, id); err != nil {
				return false, err
			}
			uuid = obj.UUID
		case "Deployment":
			obj := &api.Deployment{}
			if err := c.Client.Fetch(obj, id); err != nil {
				return false, err
			}
			uuid = obj.UUID
		case "NodeRole":
			obj := &api.NodeRole{}
			if err := c.Client.Fetch(obj, id); err != nil {
				return false, err
			}
			uuid = obj.UUID
		case "DeploymentRole":
			obj := &api.DeploymentRole{}
			if err := c.Client.Fetch(obj, id); err != nil {
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

func matchCmp(op string, val interface{}) (matcher, error) {
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
	return func(c *RunContext) (bool, error) {
		a, err := c.getVar(fixedArgs)
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

func matchScript(val interface{}) (matcher, error) {
	script, ok := val.(string)
	if !ok {
		return nil, errors.New("Script needs a string")
	}
	tmpl, err := compileScript(script)
	if err != nil {
		return nil, err
	}
	return func(c *RunContext) (bool, error) {
		return runScript(c, tmpl)
	}, nil
}

func resolveAndOr(e *Engine, r *Rule, op string, val interface{}) (matcher, error) {
	vals, ok := val.([]interface{})
	if !ok {
		return nil, fmt.Errorf("%s needs an Array", op)
	}
	res := make([]matcher, len(vals))
	for i, v := range vals {
		realV, ok := v.(map[string]interface{})
		if !ok {
			return nil, fmt.Errorf("%s member %v must be a map", op, i)
		}
		j, err := resolveMatcher(e, r, realV)
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

// ResolveMatcher compiles a map[string]interface{} with a single
// key-value pair into a function with a Matcher signature.
func resolveMatcher(e *Engine, r *Rule, m map[string]interface{}) (matcher, error) {
	if len(m) != 1 {
		return nil, fmt.Errorf("Matchers have exactly one key")
	}
	for t, v := range m {
		switch t {
		case "And", "Or":
			return resolveAndOr(e, r, t, v)
		case "Not":
			return matchNot(e, r, v)
		case "Script":
			if e.trusted {
				return nil, fmt.Errorf("Engine is trusted, Script actions not permitted")
			}
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
		default:
			return nil, fmt.Errorf("Unknown matcher %s", t)
		}
	}
	return nil, fmt.Errorf("Cannot unmarshal matcher for %#v", m)
}
