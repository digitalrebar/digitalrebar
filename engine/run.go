package engine

/*
Copyright (c) 2016, Rackn Inc.
Licensed under the terms of the Digital Rebar License.
See LICENSE.md at the top of this repository for more information.
*/

import (
	"fmt"
	"log"
	"regexp"
	"strings"

	"github.com/VictorLowther/jsonpatch/utils"
	"github.com/digitalrebar/rebar-api/client"
)

type ctx struct {
	ruleSet     RuleSet
	ruleIndexes []int
}

// RunContext holds the running information for a given Event as it matches against the Rules.
type RunContext struct {
	Engine    *Engine                // The Engine that the Event is being processed with.
	Evt       *Event                 // The Event being matched against.
	ruleStack []int                  // The stack of rule indexes that we should Return to
	ruleIdx   int                    // The index of the rule we are currently running.
	stop      bool                   // Whether we should stop processing rules
	ruleset   *RuleSet               // the Ruleset this context is processing.
	rule      *Rule                  // the rule that is currently running
	Attribs   map[string]interface{} // The DigitalRebar attributes relavent to the object that the event relates to
	Vars      map[string]interface{} // The variables that any given Matcher deems interesting.
}

// I am going to hell for this
func (c *RunContext) getVar(arg interface{}) (interface{}, error) {
	switch v := arg.(type) {
	case string:
		if strings.HasPrefix(v, `$`) {
			log.Printf("Fetching variable %s", v)
			res, ok := c.Vars[strings.TrimPrefix(v, `$`)]
			if !ok {
				return nil, fmt.Errorf("Unknown variable %s", v)
			}
			return res, nil
		}
		re := regexp.MustCompile(`^\\+\$`)
		if re.MatchString(v) {
			return strings.TrimPrefix(v, `\`), nil
		}
		return v, nil
	case []interface{}:
		res := make([]interface{}, len(v))
		for i, t := range v {
			retVal, err := c.getVar(t)
			if err != nil {
				return nil, err
			}
			res[i] = retVal
		}
		return res, nil
	case map[string]interface{}:
		res := make(map[string]interface{})
		for k, t := range v {
			retVal, err := c.getVar(t)
			if err != nil {
			}
			res[k] = retVal
		}
		return res, nil
	default:
		return arg, nil
	}
}

func (c *RunContext) clone() (*RunContext, error) {
	clonedContext := &RunContext{}
	if err := utils.Remarshal(c, &clonedContext); err != nil {
		return nil, fmt.Errorf("Unable to clone context: %v", err)
	}
	clonedContext.ruleset = c.ruleset
	clonedContext.Engine = c.Engine
	return clonedContext, nil
}

func (c *RunContext) fetchAttribs(attribs []string) error {
	if len(attribs) == 0 {
		return nil
	}
	eventID, _ := c.Evt.Event.Id()
	var attribSrc client.Attriber
	var attribSrcName string
	if c.Evt.Node != nil {
		attribSrcName = "node"
		attribSrc = c.Evt.Node
	} else if c.Evt.Deployment != nil {
		attribSrcName = "deployment"
		attribSrc = c.Evt.Deployment
	} else if c.Evt.Role != nil {
		attribSrcName = "role"
		attribSrc = c.Evt.Role
	} else {
		return fmt.Errorf("Event %s does not have attrib sources", eventID)
	}
	srcID, _ := attribSrc.Id()
	log.Printf("Using %s %s as attrib source for event %s", attribSrcName, srcID, eventID)
	c.Attribs = map[string]interface{}{}
	for _, attribName := range attribs {
		// We want an actual attrib, fetch and use it.
		attr, err := client.FetchAttrib(attribSrc, attribName, "")
		if err != nil {
			return fmt.Errorf("Error fetching attrib %s for event %s", attribName, eventID)
		}
		c.Attribs[attribName] = attr.Value
	}
	return nil
}

func (c *RunContext) processFrom(i int) {
	log.Printf("Ruleset %s matched event %s at rule %d", c.ruleset.Name, c.Evt.Selector["event"], i)
	c.ruleIdx = i
	c.ruleStack = []int{}
	c.stop = false
	for c.ruleIdx < len(c.ruleset.Rules) && !c.stop {
		c.rule = &c.ruleset.Rules[c.ruleIdx]
		if err := c.fetchAttribs(c.rule.WantsAttribs); err != nil {
			log.Printf("Ruleset %s rule %d: Error fetching attribs: %v", c.ruleset.Name, i, err)
			continue
		}
		matched, matcherr := c.rule.match(c)
		if matcherr != nil {
			log.Printf("Ruleset %s rule %d: Error matching event %s: %v",
				c.ruleset.Name,
				c.ruleIdx,
				c.Evt.Selector["name"],
				matcherr)
			c.stop = true
		}
		if matched {
			if runerr := c.rule.run(c); runerr != nil {
				log.Printf("Ruleset %s rule %d: Failed to fire properly for event %s: %v",
					c.ruleset.Name,
					c.ruleIdx,
					c.Evt.Selector["name"],
					runerr)
				c.stop = true
			}
		}
		c.ruleIdx++
	}
}
