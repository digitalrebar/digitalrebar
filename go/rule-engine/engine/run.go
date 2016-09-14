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

	"github.com/digitalrebar/go-common/event"
	rebar "github.com/digitalrebar/rebar-api/api"
)

type runErr struct {
	ruleSet string
	ruleIdx int
	err     string
}

type rcEntry struct {
	rs          RuleSet
	entrypoints []int
}

// RunContext holds the running information for a given Event as it matches against the Rules.
type RunContext struct {
	Engine    *Engine       // The Engine that the Event is being processed with.
	Evt       *event.Event  // The Event being matched against.
	Client    *rebar.Client `json:"-"` // The Client that should be used for Rebar API interactions.
	ruleStack []int         // The stack of rule indexes that we should Return to
	ruleIdx   int           // The index of the rule we are currently running.
	stop      bool          // Whether we should stop processing rules
	ruleset   *RuleSet      // the Ruleset this context is processing.
	rule      *Rule         // the rule that is currently running
	runErrors []*runErr
	toHandle  []rcEntry
	Attribs   map[string]interface{} // The DigitalRebar attributes relavent to the object that the event relates to
	Vars      map[string]interface{} // The variables that any given Matcher deems interesting.

}

func NewRunContext(e *Engine, evt *event.Event) *RunContext {
	return &RunContext{
		Engine:    e,
		Evt:       evt,
		runErrors: []*runErr{},
	}
}

func (c *RunContext) AddRuleSet(rs RuleSet, entrypoints []int) {
	c.toHandle = append(c.toHandle, rcEntry{rs, entrypoints})
}

func (c *RunContext) Error() string {
	res := make([]string, len(c.runErrors))
	for i, e := range c.runErrors {
		res[i] = fmt.Sprintf("ruleset %s rule %d err: %v", e.ruleSet, e.ruleIdx, e.err)
	}
	return strings.Join(res, "\n")
}

func (c *RunContext) logString(str string, args ...interface{}) string {
	return fmt.Sprintf("Event %s: Ruleset %s: %s:",
		c.Evt.Event.UUID,
		c.ruleset.Name,
		fmt.Sprintf(str, args...))
}

func (c *RunContext) log(str string, args ...interface{}) {
	log.Println(c.logString(str, args...))
}

func (c *RunContext) err(idx int, str string, args ...interface{}) {
	logStr := c.logString(str, args...)
	log.Println(logStr)
	c.runErrors = append(c.runErrors, &runErr{c.ruleset.Name, idx, logStr})
}

// I am going to hell for this
func (c *RunContext) getVar(arg interface{}) (interface{}, error) {
	switch v := arg.(type) {
	case string:
		if strings.HasPrefix(v, `$`) {
			res, ok := c.Vars[strings.TrimPrefix(v, `$`)]
			if !ok {
				c.err(c.ruleIdx, "Unknown variable %s", v)
				return nil, c
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
				c.err(c.ruleIdx, "%v", err)
				return nil, c
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

func (c *RunContext) fetchAttribs(attribs []string) error {
	if len(attribs) == 0 {
		return nil
	}
	eventID, _ := c.Evt.Event.Id()
	var attribSrc rebar.Attriber
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
		c.err(c.ruleIdx, "Event %s does not have attrib sources", eventID)
		return c
	}
	srcID, _ := attribSrc.Id()
	c.log("Using %s %s as attrib source", attribSrcName, srcID)
	c.Attribs = map[string]interface{}{}
	for _, attribName := range attribs {
		// We want an actual attrib, fetch and use it.
		attr, err := c.Client.FetchAttrib(attribSrc, attribName, "")
		if err != nil {
			c.err(c.ruleIdx, "Error fetching attrib %s for event %s", attribName, eventID)
			return c
		}
		c.Attribs[attribName] = attr.Value
	}
	return nil
}

func (c *RunContext) processFrom(i int) {
	c.log("matched at rule %d", i)
	c.ruleIdx = i
	c.ruleStack = []int{}
	c.stop = false
	c.Vars = make(map[string]interface{})
	for c.ruleIdx < len(c.ruleset.Rules) && !c.stop {
		ruleIdx := c.ruleIdx
		c.rule = &c.ruleset.Rules[ruleIdx]
		if err := c.fetchAttribs(c.rule.WantsAttribs); err != nil {
			continue
		}
		matched, matcherr := c.rule.match(c)
		if matcherr != nil {
			c.err(ruleIdx, "Match error: %v", matcherr)
			c.stop = true
		}
		if matched {
			c.log("Rule %d: Matched, running actions", ruleIdx)
			if runerr := c.rule.run(c); runerr != nil {
				c.err(ruleIdx, "Failed to fire: %v", runerr)
				c.stop = true
			}
		} else {
			c.log("Rule %d did not match", ruleIdx)
		}
		c.ruleIdx++
		if c.stop {
			c.log("Rule %d: Stopping", ruleIdx)
		} else {
			c.log("Rule %d: continuing to rule %d", ruleIdx, c.ruleIdx)
		}

	}
}

func (c *RunContext) Process() error {
	for ent := range c.toHandle {
		c.ruleset = &c.toHandle[ent].rs
		var client *rebar.Client
		if c.Engine.trusted {
			if c.ruleset.Username == "" {
				log.Panicf("Cannot happen: trusted client with no username")
			}
			var err error
			client, err = rebar.TrustedSession(c.Engine.Client.URL, c.ruleset.Username)
			if err != nil {
				log.Panicf("Failed to establis trusted session impersonating %s", c.ruleset.Username)
			}
		} else {
			client = c.Engine.Client
		}
		c.Client = client
		for _, i := range c.toHandle[ent].entrypoints {
			c.processFrom(i)
		}
	}
	if len(c.runErrors) > 0 {
		return c
	}
	return nil
}
