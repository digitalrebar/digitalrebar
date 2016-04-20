package main

import (
	"fmt"
	"log"
	"regexp"
	"strings"

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

func (c *RunContext) getVar(arg interface{}) (interface{}, error) {
	varRef, ok := arg.(string)
	if !ok {
		return arg, nil
	}
	if strings.HasPrefix(varRef, `$`) {
		log.Printf("Fetching variable %s", varRef)
		arg, ok := c.Vars[strings.TrimPrefix(varRef, `$`)]
		if !ok {
			return nil, fmt.Errorf("Unknown variable %s", varRef)
		}
		return arg, nil
	}
	re := regexp.MustCompile(`^\\+\$`)
	if re.MatchString(varRef) {
		return strings.TrimPrefix(varRef, `\`), nil
	}
	return arg, nil
}

func (c *RunContext) Clone() (*RunContext, error) {
	clonedContext := &RunContext{}
	if err := utils.Remarshal(c, &clonedContext); err != nil {
		return nil, fmt.Errorf("Unable to clone context: %v", err)
	}
	clonedContext.rule = c.rule
	return clonedContext, nil
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
