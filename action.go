package main

import (
	"errors"
	"fmt"
	"log"
)

var actionTypes = []string{"Log", "Script"}

//Action is a thing that the Classifier will do for a Rule once the
//Rule determines whether it should fire.
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

func actionScript(script string) Action {
	return func(e *Event) error {
		res, err := runScript(e, script)
		if err != nil {
			return err
		}
		if !res {
			return errors.New("Script run failed")
		}
		return nil
	}
}

func resolveAction(a map[string]interface{}) (Action, error) {
	log.Printf("Resolving action %#v", a)
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
		case "Script":
			j, ok := a[t].(string)
			if !ok {
				return nil, fmt.Errorf("%s needs a string", t)
			}
			return actionScript(j), nil
		default:
			return nil, fmt.Errorf("Unknown action %s", t)
		}
	}
	return nil, fmt.Errorf("Cannot unmarshal action for #%v", a)
}
