package main

import (
	"fmt"
	"log"

	"github.com/VictorLowther/jsonpatch/utils"
	"github.com/digitalrebar/rebar-api/client"
)

// RunContext holds the running information for a given Event as it matches against the Rules.
type RunContext struct {
	Evt     *Event // The Event being matched against.
	rule    *Rule
	Attribs map[string]interface{} // The DigitalRebar attributes relavent to the object that the event relates to
	Vars    map[string]interface{} // The variables that any given Matcher deems interesting.
}

func makeContext(e *Event) *RunContext {
	return &RunContext{Evt: e}
}

func cloneContext(c *RunContext) *RunContext {
	clonedContext := &RunContext{}
	utils.Remarshal(c, &clonedContext)
	return clonedContext
}

func (e *RunContext) fetchAttribs() error {
	if len(e.rule.WantsAttribs) == 0 {
		return nil
	}
	eventId, _ := e.Evt.Event.Id()
	var attribSrc client.Attriber
	var attribSrcName string
	if e.Evt.Node != nil {
		attribSrcName = "node"
		attribSrc = e.Evt.Node
	} else if e.Evt.Deployment != nil {
		attribSrcName = "deployment"
		attribSrc = e.Evt.Deployment
	} else if e.Evt.Role != nil {
		attribSrcName = "role"
		attribSrc = e.Evt.Role
	} else {
		return fmt.Errorf("Event %s does not have attrib sources", eventId)
	}
	srcId, _ := attribSrc.Id()
	log.Printf("Using %s %s as attrib source for event %s", attribSrcName, srcId, eventId)
	e.Attribs = map[string]interface{}{}
	for _, attribName := range e.rule.WantsAttribs {
		// We want an actual attrib, fetch and use it.
		attr, err := client.FetchAttrib(attribSrc, attribName, "")
		if err != nil {
			return fmt.Errorf("Error fetching attrib %s for event %s", attribName, eventId)
		}
		e.Attribs[attribName] = attr.Value
	}
	return nil
}

// Process the Rules against an event.  This implementation is very
// slow and stupid, and will be replaced if it starts being a
// bottleneck.
func (e *RunContext) Process(rules []*Rule) {
	ruleLock.Lock()
	defer ruleLock.Unlock()
	for _, rule := range rules {
		e.rule = rule
		if err := e.fetchAttribs(); err != nil {
			log.Printf("Error fetching attribs: %v", err)
			continue
		}
		e.Vars = make(map[string]interface{})
		matched, matcherr, runerr := rule.Fire(e)
		log.Printf("Rule %s matched event %s: %v",
			rule.Name,
			e.Evt.Selector["name"],
			matched)
		if matcherr != nil {
			log.Printf("Rule %s had an error matching event %s: %v",
				rule.Name,
				e.Evt.Selector["name"],
				matcherr)
		}
		if runerr != nil {
			log.Printf("Rule %s failed to fire properly for event %s: %v",
				rule.Name,
				e.Evt.Selector["name"],
				runerr)
		}

	}
}
