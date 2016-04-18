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
	return func(e *RunContext) error {
		res, err := runScript(e, script)
		if err != nil {
			return err
		}
		if !res {
			return errors.New("Script run failed")
		}
		return nil
	}, nil
}

func resolveAction(a map[string]interface{}) (Action, error) {
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
