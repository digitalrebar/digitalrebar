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

	"github.com/digitalrebar/rebar-api/client"
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

func commitThis(thing client.Attriber, id string) error {
	if err := client.Fetch(thing, id); err != nil {
		return err
	}
	return client.Commit(thing)
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
			node := &client.Node{}
			return commitThis(node, id)
		case "DeploymentID":
			deployment := &client.Deployment{}
			return commitThis(deployment, id)
		case "DeploymentRoleID":
			deploymentRole := &client.DeploymentRole{}
			return commitThis(deploymentRole, id)
		case "NodeRoleID":
			nodeRole := &client.NodeRole{}
			return commitThis(nodeRole, id)
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
	node := &client.Node{}
	role := &client.Role{}
	if err := client.Fetch(node, nodeID); err != nil {
		return fmt.Errorf("Failed to load Node with id %s: %v", nodeID, err)
	}
	if err := client.Fetch(role, roleID); err != nil {
		return fmt.Errorf("Failed to load Role with id %s: %v", roleID, err)
	}
	nr := &client.NodeRole{}
	if err := client.Init(nr); err != nil {
		return fmt.Errorf("Failed to initialize NodeRole: %v", err)
	}
	nr.RoleID = role.ID
	nr.NodeID = node.ID
	nr.DeploymentID = node.DeploymentID
	if err := client.BaseCreate(nr); err != nil {
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

func moveNode(nodeID, deplID string) error {
	node := &client.Node{}
	deployment := &client.Deployment{}
	if err := client.Fetch(node, nodeID); err != nil {
		return fmt.Errorf("Failed to fetch Node %s: %v", nodeID, err)
	}
	if err := client.Fetch(deployment, deplID); err != nil {
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
	deployment := &client.Deployment{}
	role := &client.Role{}
	if err := client.Fetch(deployment, deplID); err != nil {
		return fmt.Errorf("Failed to fetch deployement %s: %v", deplID, err)
	}
	if err := client.Fetch(role, roleID); err != nil {
		return fmt.Errorf("Failed to fetch role %s: %v", roleID, err)
	}
	dr := &client.DeploymentRole{}
	dr.DeploymentID = deployment.ID
	dr.RoleID = role.ID
	if err := client.BaseCreate(dr); err != nil {
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
			return moveNode(nodeID, deplID)
		} else if roleOK && deplOK {
			return bindDeploymentRole(c, deplID, roleID, saveAs)
		}
		log.Panic("Cannot happen")
		return nil
	}, nil
}

func retryNodeRole(c *RunContext, nodeRoleID string) error {
	nr := &client.NodeRole{}
	if err := client.Fetch(nr, nodeRoleID); err != nil {
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
		var tgt client.Attriber
		attrib := &client.Attrib{}

		for k, fv := range fixedVals {

			switch k {
			case "Attrib":
				attrID, attrOk := fv.(string)
				if !attrOk {
					return fmt.Errorf("Attrib id %#v is not a string.", fv)
				}
				if err := client.Fetch(attrib, attrID); err != nil {
					return err
				}
			case "Value":
				attrVal = fv
			case "NodeID":
				tgt = &client.Node{}
				id, ok = fv.(string)
			case "DeploymentID":
				tgt = &client.Deployment{}
				id, ok = fv.(string)
			case "DeploymentRoleID":
				tgt = &client.DeploymentRole{}
				id, ok = fv.(string)
			case "NodeRoleID":
				tgt = &client.NodeRole{}
				id, ok = fv.(string)
			default:
				log.Panicf("SetAttrib: cannot happen processing %s", k)
			}
		}
		if !ok {
			return fmt.Errorf("SetAttrib: Failed to get ID of thing to set attrib on")
		}
		attrib.Value = attrVal
		if err := client.Fetch(tgt, id); err != nil {
			return err
		}
		return client.SetAttrib(tgt, attrib, "")
	}, nil
}

func actionDelay(val interface{}) (action, error) {
	secs, ok := val.(int)
	if !ok {
		return nil, errors.New("Delay needs the number of seconds to delay")
	}
	return func(c *RunContext) error {
		time.Sleep(time.Duration(secs) * time.Second)
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

// ResolveAction compiles a map[string]interface{} with a single
// key-value pair into a function matching the Action type.
// Recognized keys are:
//
// "Log", which causes the engine to emit a hard-coded logging message
// to stderr.  This is mainly for testing purposes so far, but we may
// repurpose it to emit something useful later.
//
// "Script", which expects its value to be a string that will be
// parsed using text.Template.  The result of that parsing should be a
// valid bash script.  When the Action is ran, the parsed script will
// be compiled using the passed RunContext.  If the compilation fails,
// or the resultant script executes with a non-zero exit status, the
// Action will fail, otherwise it will pass.
//
// "Delay", which causes the a delay of duration seconds for doing the
// encapsulated actions described above.
//
// "Bind, which expects its value to be a struct matching the following format:
//    struct {
//        NodeID string
//        DeploymentID string
//        RoleID string
//        SaveAs string
//    }
// Exactly 2 of the ID fields must be filled, and which two determine what action Bind will take.
//
// "NodeID" and "RoleID": a Role will be bound to a Node
//
// "NodeID" and "DeploymentID": a Node will be moved into a new Deployment
//
// "RoleID" and "DeploymentID": a Role will be bound to a Deployment
//
// If "SaveAs is set, the resultant new object's unique identifier (if
// one was created) will be saved in the referenced variable
//
// "Retry", which causes a node role to be retried.
//   struct {
//     NodeRoleID string
//   }
// This causes the node role to be retried.
//
// "SetAttrib", which expects its value to be a struct wuth the following format:
//
//    struct {
//        Attrib string
//        Node string
//        Deployment string
//        NodeRole string
//        DeploymentRole string
//        Value interface{}
//    }
//
//
// Out if those fields, exactly one of Node, Role, Deployment,
// NodeRole, and DeploymentRole should be filled, and their values
// must resolve to an existing object of their type, otherwise the
// action will fail.  Attrib must be the name of the attribute to set,
// and Value must be tbe value you want to set the attrib to for the
// object.
func resolveAction(rs *RuleSet, ruleIdx int, a map[string]interface{}) (action, error) {
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
		default:
			return nil, fmt.Errorf("Unknown action %s", t)
		}
	}
	return nil, fmt.Errorf("Cannot unmarshal action for #%v", a)
}
