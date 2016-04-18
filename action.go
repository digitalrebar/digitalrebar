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

// ResolveAction compiles a map[string]interface{} with a single key-value pair into a function matching
// the Action type.  Recognized keys are:
//
// "Log", which causes the engine to emit a hard-coded logging message to stderr.  This is mainly for testing
// purposes so far, but we may repurpose it to emit something useful later.
//
// "Script", which expects its value to be a string that will be parsed using text.Template.  The result of that parsing should
// be a valid bash script.  When the Action is ran, the parsed script will be compiled using the passed RunContext.  If the compilation fails,
// or the resultant script executes with a non-zero exit status, the Action will fail, otherwise it will pass.
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
		default:
			return nil, fmt.Errorf("Unknown action %s", t)
		}
	}
	return nil, fmt.Errorf("Cannot unmarshal action for #%v", a)
}
