package main

import (
	"fmt"
	"log"
)

var actionTypes = []string{"Log"}

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
