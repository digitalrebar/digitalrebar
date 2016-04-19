package main

import (
	"errors"
	"fmt"
	"log"
)

//Action is a thing that the Classifier will do for a Rule once the
//Rule determines whether it should fire.
type Action func(*RunContext) error

func actionLog() (Action, error) {
	return func(e *RunContext) error {
		log.Printf("Event %s matched rule %s for node %s",
			e.Evt.Selector["event"],
			e.rule.Name,
			e.Evt.Node.Name)
		return nil
	}, nil
}

func actionScript(val interface{}) (Action, error) {
	script, ok := val.(string)
	if !ok {
		return nil, errors.New("Script needs a string")
	}
	tmpl, err := compileScript(script)
	if err != nil {
		return nil, err
	}
	return func(e *RunContext) error {
		res, err := runScript(e, tmpl)
		if err != nil {
			return err
		}
		if !res {
			return errors.New("Script run failed")
		}
		return nil
	}, nil
}

func actionDelay(val interface{}) (Action, error) {
	array, ok := val.([]interface{})
	if !ok {
		return nil, errors.New("Delay needs nested actions")
	}
	if len(array) < 2 {
		return nil, errors.New("Delay needs at least one nested action and a duration")
	}

	jj := array[0].(map[string]interface{})

	// First element must be a map with duration integer
	dobj, ok := jj["Duration"]
	if !ok {
		return nil, errors.New("Delay needs a map with duration as first element")
	}
	duration, ok := dobj.(float64)
	if !ok {
		return nil, errors.New("Duration must contain an integer")
	}

	actions := make([]Action, 0, 0)
	// The remaining elements must compile to actions
	for _, e := range array[1:] {
		element := e.(map[string]interface{})
		subaction, err := ResolveAction(element)
		if err != nil {
			return nil, err
		}
		actions = append(actions, subaction)
	}

	return func(e *RunContext) error {
		delay_actions(e, int(duration), actions)
		return nil
	}, nil
}

// ResolveAction compiles a map[string]interface{} with a single key-value pair into a function matching
// the Action type.  Recognized keys are:
//
// "Log", which causes the engine to emit a hard-coded logging message to stderr.  This is mainly for testing
// purposes so far, but we may repurpose it to emit something useful later.
//
// "Script", which expects its value to be a string that will be parsed using text.Template.  The result of that parsing should
// be a valid bash script.  When the Action is ran, the parsed script will be compiled using the passed RunContext.  If the compilation fails,
// or the resultant script executes with a non-zero exit status, the Action will fail, otherwise it will pass.
//
// "Delay", which causes the a delay of duration seconds for doing the encapsulated actions described above.
func ResolveAction(a map[string]interface{}) (Action, error) {
	if len(a) != 1 {
		return nil, fmt.Errorf("Actions have exactly one key")
	}
	for t, v := range a {
		switch t {
		case "Log":
			return actionLog()
		case "Script":
			return actionScript(v)
		case "Delay":
			return actionDelay(v)
		default:
			return nil, fmt.Errorf("Unknown action %s", t)
		}
	}
	return nil, fmt.Errorf("Cannot unmarshal action for #%v", a)
}
