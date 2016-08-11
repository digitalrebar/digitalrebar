package engine

/*
Copyright (c) 2016, Rackn Inc.
Licensed under the terms of the Digital Rebar License.
See LICENSE.md at the top of this repository for more information.
*/

import (
	"errors"
	"fmt"
	"log"
	"time"

	"github.com/digitalrebar/rebar-api/api"
)

type action func(*RunContext) error

func actionLog() (action, error) {
	return func(c *RunContext) error {
		log.Printf("Event %s matched ruleset %s for node %s",
			c.Evt.Selector["event"],
			c.ruleset.Name,
			c.Evt.Node.Name)
		return nil
	}, nil
}

func actionScript(val interface{}) (action, error) {
	script, ok := val.(string)
	if !ok {
		return nil, errors.New("Script needs a string")
	}
	tmpl, err := compileScript(script)
	if err != nil {
		return nil, err
	}
	return func(c *RunContext) error {
		res, err := runScript(c, tmpl)
		if err != nil {
			return err
		}
		if !res {
			return errors.New("Script run failed")
		}
		return nil
	}, nil
}

func commitThis(c *RunContext, thing api.Attriber, id string) error {
	if err := c.Client.Fetch(thing, id); err != nil {
		return err
	}
	return c.Client.Commit(thing)
}

func commitThing(thing, val string) (action, error) {
	return func(c *RunContext) error {
		ref, err := c.getVar(val)
		if err != nil {
			return err
		}
		id, ok := ref.(string)
		if !ok {
			return fmt.Errorf("commit: %s id %v is not a string", thing, ref)
		}
		switch thing {
		case "NodeID":
			node := &api.Node{}
			return commitThis(c, node, id)
		case "DeploymentID":
			deployment := &api.Deployment{}
			return commitThis(c, deployment, id)
		case "DeploymentRoleID":
			deploymentRole := &api.DeploymentRole{}
			return commitThis(c, deploymentRole, id)
		case "NodeRoleID":
			nodeRole := &api.NodeRole{}
			return commitThis(c, nodeRole, id)
		default:
			log.Panicf("Case %s cannot happen!", thing)
		}
		return nil
	}, nil
}

func actionCommit(val interface{}) (action, error) {
	toCommit, ok := val.(map[string]interface{})
	if !ok || len(toCommit) != 1 {
		return nil, fmt.Errorf("Expected a single thing to commit, got %d: %#v", len(toCommit), toCommit)
	}

	for k, v := range toCommit {
		val, ok := v.(string)
		if !ok {
			return nil, fmt.Errorf("Thing to commit must be a string, not %#v", v)
		}
		switch k {
		case "NodeID", "DeploymentID", "DeploymentRoleID", "NodeRoleID":
			return commitThing(k, val)
		default:
			return nil, fmt.Errorf("Cannot commit %s", k)
		}
	}
	log.Panic("Cannot happen in actionCommit")
	return nil, nil
}

func bindNodeRole(c *RunContext, nodeID, roleID, saveAs string) error {
	node := &api.Node{}
	role := &api.Role{}
	if err := c.Client.Fetch(node, nodeID); err != nil {
		return fmt.Errorf("Failed to load Node with id %s: %v", nodeID, err)
	}
	if err := c.Client.Fetch(role, roleID); err != nil {
		return fmt.Errorf("Failed to load Role with id %s: %v", roleID, err)
	}
	nr := &api.NodeRole{}
	if err := c.Client.Init(nr); err != nil {
		return fmt.Errorf("Failed to initialize NodeRole: %v", err)
	}
	nr.RoleID = role.ID
	nr.NodeID = node.ID
	nr.DeploymentID = node.DeploymentID
	if err := c.Client.BaseCreate(nr); err != nil {
		return fmt.Errorf("Failed to create noderole for node: %s role %s: %v",
			nodeID,
			roleID,
			err)
	}
	if saveAs != "" {
		c.Vars[saveAs], _ = nr.Id()
	}
	return nil
}

func moveNode(c *RunContext, nodeID, deplID string) error {
	node := &api.Node{}
	deployment := &api.Deployment{}
	if err := c.Client.Fetch(node, nodeID); err != nil {
		return fmt.Errorf("Failed to fetch Node %s: %v", nodeID, err)
	}
	if err := c.Client.Fetch(deployment, deplID); err != nil {
		return fmt.Errorf("Failed to fetch deployment %s: %v", deplID, err)
	}
	if err := node.Move(deployment); err != nil {
		return fmt.Errorf("Failed to move node %s to deployment %s: %v",
			nodeID,
			deplID,
			err)
	}
	return nil
}

func bindDeploymentRole(c *RunContext, deplID, roleID, saveAs string) error {
	deployment := &api.Deployment{}
	role := &api.Role{}
	if err := c.Client.Fetch(deployment, deplID); err != nil {
		return fmt.Errorf("Failed to fetch deployement %s: %v", deplID, err)
	}
	if err := c.Client.Fetch(role, roleID); err != nil {
		return fmt.Errorf("Failed to fetch role %s: %v", roleID, err)
	}
	dr := &api.DeploymentRole{}
	dr.DeploymentID = deployment.ID
	dr.RoleID = role.ID
	if err := c.Client.BaseCreate(dr); err != nil {
		return fmt.Errorf("Failed to create deploymentrole for deployment %s: role %s: %v",
			deplID,
			roleID,
			err)
	}
	if saveAs != "" {
		c.Vars[saveAs], _ = dr.Id()
	}
	return nil
}

func actionBind(val interface{}) (action, error) {
	b, ok := val.(map[string]interface{})
	if !ok {
		return nil, fmt.Errorf("Error extracting binding %#v", b)
	}
	present := 0
	for k, v := range b {
		switch k {
		case "NodeID", "RoleID", "DeploymentID":
			present++
		case "SaveAs":
			x, ok := v.(string)
			if !ok {
				return nil, fmt.Errorf("SaveAs must be a string")
			}
			b[k] = x
		default:
			return nil, fmt.Errorf("bind: Unknown key %s", k)
		}
	}
	if present != 2 {
		return nil, fmt.Errorf("Bind action requires 2 things to bind, got %d: %#v", present, b)
	}
	return func(c *RunContext) error {
		vars, err := c.getVar(b)
		if err != nil {
			return err
		}
		v := vars.(map[string]interface{})
		var roleOK, deplOK, nodeOK bool
		var roleID, deplID, nodeID, saveAs string
		for k, val := range v {
			switch k {
			case "NodeID":
				nodeID, nodeOK = val.(string)
			case "RoleID":
				roleID, roleOK = val.(string)
			case "DeploymentID":
				deplID, deplOK = val.(string)
			case "SaveAs":
				saveAs, _ = val.(string)
			default:
				log.Panicf("Cannot happen: %s", k)
			}
		}
		if nodeOK && roleOK {
			return bindNodeRole(c, nodeID, roleID, saveAs)
		} else if nodeOK && deplOK {
			return moveNode(c, nodeID, deplID)
		} else if roleOK && deplOK {
			return bindDeploymentRole(c, deplID, roleID, saveAs)
		}
		log.Panic("Cannot happen")
		return nil
	}, nil
}

func retryNodeRole(c *RunContext, nodeRoleID string) error {
	nr := &api.NodeRole{}
	if err := c.Client.Fetch(nr, nodeRoleID); err != nil {
		return fmt.Errorf("Failed to load NodeRole with id %s: %v", nodeRoleID, err)
	}
	if err := nr.Retry(); err != nil {
		return fmt.Errorf("Failed to retry noderole: %s: %v",
			nodeRoleID,
			err)
	}
	return nil
}

func actionRetry(val interface{}) (action, error) {
	b, ok := val.(map[string]interface{})
	if !ok {
		return nil, fmt.Errorf("Error extracting retry binding %#v", b)
	}
	present := 0
	for k := range b {
		switch k {
		case "NodeRoleID":
			present++
		default:
			return nil, fmt.Errorf("retry: Unknown key %s", k)
		}
	}
	if present != 1 {
		return nil, fmt.Errorf("Retry action requires 1 thing to retry, got %d: %#v", present, b)
	}
	return func(c *RunContext) error {
		vars, err := c.getVar(b)
		if err != nil {
			return err
		}
		v := vars.(map[string]interface{})
		var nodeRoleOK bool
		var nodeRoleID string
		for k, val := range v {
			switch k {
			case "NodeRoleID":
				nodeRoleID, nodeRoleOK = val.(string)
			default:
				log.Panicf("Cannot happen: %s", k)
			}
		}
		if nodeRoleOK {
			return retryNodeRole(c, nodeRoleID)
		}
		log.Panic("Cannot happen")
		return nil
	}, nil
}

func setAttrib(val interface{}) (action, error) {
	vals, ok := val.(map[string]interface{})
	if !ok {
		return nil, fmt.Errorf("SetAttrib: Expected a map, got %#v", val)
	}
	var seenAttribName, seenVal bool
	var tgt string
	for k := range vals {
		switch k {
		case "NodeID", "DeploymentID", "DeploymentRoleID", "NodeRoleID":
			if tgt != "" {
				return nil, fmt.Errorf("SetAttrib: Already targeting %s, will not target %s", tgt, k)
			}
			tgt = k
		case "Attrib":
			seenAttribName = true
		case "Value":
			seenVal = true
		default:
			return nil, fmt.Errorf("SetVal: unknown parameter %s", k)
		}
	}
	if tgt == "" || !seenAttribName || !seenVal {
		return nil, fmt.Errorf("SetAttrib: Malformed argument: #%v", vals)
	}
	return func(c *RunContext) error {
		val, err := c.getVar(vals)
		if err != nil {
			return err
		}
		fixedVals, ok := val.(map[string]interface{})
		if !ok {
			return fmt.Errorf("SetAttrib: Var resolution failed for %#v", val)
		}
		var attrVal interface{}
		var id string
		var tgt api.Attriber
		attrib := &api.Attrib{}

		for k, fv := range fixedVals {

			switch k {
			case "Attrib":
				attrID, attrOk := fv.(string)
				if !attrOk {
					return fmt.Errorf("Attrib id %#v is not a string.", fv)
				}
				if err := c.Client.Fetch(attrib, attrID); err != nil {
					return err
				}
			case "Value":
				attrVal = fv
			case "NodeID":
				tgt = &api.Node{}
				id, ok = fv.(string)
			case "DeploymentID":
				tgt = &api.Deployment{}
				id, ok = fv.(string)
			case "DeploymentRoleID":
				tgt = &api.DeploymentRole{}
				id, ok = fv.(string)
			case "NodeRoleID":
				tgt = &api.NodeRole{}
				id, ok = fv.(string)
			default:
				log.Panicf("SetAttrib: cannot happen processing %s", k)
			}
		}
		if !ok {
			return fmt.Errorf("SetAttrib: Failed to get ID of thing to set attrib on")
		}
		attrib.Value = attrVal
		if err := c.Client.Fetch(tgt, id); err != nil {
			return err
		}
		return c.Client.SetAttrib(tgt, attrib, "")
	}, nil
}

func actionDelay(val interface{}) (action, error) {
	secs, ok := val.(float64)
	if !ok {
		return nil, fmt.Errorf("Delay got %T %#v, needed a float64", val, val)
	}
	return func(c *RunContext) error {
		time.Sleep(time.Duration(int(secs)) * time.Second)
		return nil
	}, nil
}

func actionStop() (action, error) {
	return func(c *RunContext) error {
		c.stop = true
		return nil
	}, nil
}

func actionReturn() (action, error) {
	return func(c *RunContext) error {
		if len(c.ruleStack) == 0 {
			c.stop = true
		} else {
			c.ruleIdx = c.ruleStack[len(c.ruleStack)-1]
			c.ruleStack = c.ruleStack[:len(c.ruleStack)-1]
		}
		return nil
	}, nil
}

func actionJumpOrCall(rs *RuleSet, ruleIdx int, call bool, v interface{}) (action, error) {
	tgt, ok := v.(string)
	if !ok {
		return nil, fmt.Errorf("Jump/Call argument not a string")
	}
	tgtIdx, ok := rs.namedRules[tgt]
	if !ok {
		return nil, fmt.Errorf("'%s' does not refer to a rule in ruleset '%s'", tgt, rs.Name)
	}
	if tgtIdx <= ruleIdx {
		return nil, fmt.Errorf("Invalid jump/call from rule %d to %d (%s). Jumps must go forward", ruleIdx, tgtIdx, tgt)

	}
	log.Printf("Ruleset %s: Compiling call/jump from %d to %d (%s)",
		rs.Name,
		ruleIdx,
		tgtIdx,
		tgt)
	return func(c *RunContext) error {
		if call {
			c.ruleStack = append(c.ruleStack, ruleIdx)
		}
		// subtract 1 here because the main run loop will add one back to it.
		c.ruleIdx = tgtIdx - 1
		return nil
	}, nil
}

func actionNode(v interface{}) (action, error) {
	tgt, ok := v.(map[string]interface{})
	if !ok {
		return nil, fmt.Errorf("Node requires a map")
	}
	uuidThing, ok := tgt["UUID"]
	if !ok {
		return nil, fmt.Errorf("Node needs a UUID element")
	}
	uuid, ok := uuidThing.(string)
	if !ok {
		return nil, fmt.Errorf("Node UUID must be a string")
	}
	actionThing, ok := tgt["Action"]
	if !ok {
		return nil, fmt.Errorf("Node needs an Action element")
	}
	action, ok := actionThing.(string)
	if !ok {
		return nil, fmt.Errorf("Node action must be a string")
	}
	switch action {
	case "Scrub", "Redeploy", "Propose", "Destroy", "Commit":
	default:
		return nil, fmt.Errorf("Unknown node action %s", action)
	}
	return func(c *RunContext) error {
		node := &api.Node{}
		if err := c.Client.Fetch(node, uuid); err != nil {
			return err
		}
		var err error
		switch action {
		case "Scrub":
			err = node.Scrub()
		case "Redeploy":
			err = node.Redeploy()
		case "Propose":
			err = c.Client.Propose(node)
		case "Destroy":
			err = c.Client.Destroy(node)
		case "Commit":
			err = c.Client.Commit(node)
		default:
			err = fmt.Errorf("Cannot happen: node action %s", action)
		}
		return err
	}, nil
}

func resolveAction(e *Engine, rs *RuleSet, ruleIdx int, a map[string]interface{}) (action, error) {
	if len(a) != 1 {
		return nil, fmt.Errorf("Actions have exactly one key")
	}
	for t, v := range a {
		switch t {
		case "Log":
			return actionLog()
		case "Script":
			if e.trusted {
				return nil, fmt.Errorf("Engine is trusted, Script actions not permitted")
			}
			return actionScript(v)
		case "Delay":
			return actionDelay(v)
		case "Bind":
			return actionBind(v)
		case "Retry":
			return actionRetry(v)
		case "Commit":
			return actionCommit(v)
		case "SetAttrib":
			return setAttrib(v)
		case "Stop":
			return actionStop()
		case "Return":
			return actionReturn()
		case "Jump":
			return actionJumpOrCall(rs, ruleIdx, false, v)
		case "Call":
			return actionJumpOrCall(rs, ruleIdx, true, v)
		case "Node":
			return actionNode(v)
		default:
			return nil, fmt.Errorf("Unknown action %s", t)
		}
	}
	return nil, fmt.Errorf("Cannot unmarshal action for #%v", a)
}
