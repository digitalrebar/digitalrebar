package main

import (
	"errors"
	"fmt"
	"log"

	"github.com/VictorLowther/jsonpatch/utils"
	"github.com/digitalrebar/rebar-api/client"
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

func commitThis(thing client.Attriber, id string) error {
	if err := client.Fetch(thing, id); err != nil {
		return err
	}
	return client.Commit(thing)
}

func commitThing(thing, val string) (Action, error) {
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

func actionCommit(val interface{}) (Action, error) {
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

type binder struct {
	NodeID       string
	RoleID       string
	DeploymentID string
	SaveAs       string
}

func normalizeRefs(c *RunContext, a, b string) (d, e string, err error) {
	var aRef, bRef interface{}
	d, e = a, b
	aRef, err = c.getVar(a)
	if err == nil {
		bRef, err = c.getVar(b)
	}
	if err != nil {
		return a, b, err
	}
	var ok bool
	d, ok = aRef.(string)
	if !ok {
		return a, b, fmt.Errorf("normalizeRefs: %#v is not a string", d)
	}
	e, ok = bRef.(string)
	if !ok {
		return a, b, fmt.Errorf("normalizeRefs: %#v is not a string", d)
	}
	return
}

func bindNodeRole(b *binder) (Action, error) {
	return func(c *RunContext) error {
		nodeID, roleID, err := normalizeRefs(c, b.NodeID, b.RoleID)
		if err != nil {
			return fmt.Errorf("bindNodeRole: Lookup failed for %#v: %v", b, err)
		}

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
		if b.SaveAs != "" {
			c.Vars[b.SaveAs], _ = nr.Id()
		}
		return nil
	}, nil

}

func moveNode(b *binder) (Action, error) {
	return func(c *RunContext) error {
		nodeID, deploymentID, err := normalizeRefs(c, b.NodeID, b.DeploymentID)
		if err != nil {
			return fmt.Errorf("moveNode: Lookup failed for %#v: %v", b, err)
		}
		node := &client.Node{}
		deployment := &client.Deployment{}
		if err := client.Fetch(node, nodeID); err != nil {
			return fmt.Errorf("Failed to fetch Node %s: %v", nodeID, err)
		}
		if err := client.Fetch(deployment, deploymentID); err != nil {
			return fmt.Errorf("Failed to fetch deployment %s: %v", deploymentID, err)
		}
		if err := node.Move(deployment); err != nil {
			return fmt.Errorf("Failed to move node %s to deployment %s: %v",
				nodeID,
				deploymentID,
				err)
		}
		return nil
	}, nil
}

func bindDeploymentRole(b *binder) (Action, error) {
	return func(c *RunContext) error {
		deploymentID, roleID, err := normalizeRefs(c, b.DeploymentID, b.RoleID)
		if err != nil {
			return fmt.Errorf("bindDeploymentRole: Lookup failed for %#v: %v", b, err)
		}
		deployment := &client.Deployment{}
		role := &client.Role{}
		if err := client.Fetch(deployment, deploymentID); err != nil {
			return fmt.Errorf("Failed to fetch deployement %s: %v", deploymentID, err)
		}
		if err := client.Fetch(role, roleID); err != nil {
			return fmt.Errorf("Failed to fetch role %s: %v", roleID, err)
		}
		dr := &client.DeploymentRole{}
		dr.DeploymentID = deployment.ID
		dr.RoleID = role.ID
		if err := client.BaseCreate(dr); err != nil {
			return fmt.Errorf("Failed to create deploymentrole for deployment %s: role %s: %v",
				deploymentID,
				roleID,
				err)
		}
		if b.SaveAs != "" {
			c.Vars[b.SaveAs], _ = dr.Id()
		}
		return nil
	}, nil
}

func actionBind(val interface{}) (Action, error) {
	b := &binder{}
	if err := utils.Remarshal(val, &b); err != nil {
		return nil, fmt.Errorf("Error extracting binding: %v", err)
	}
	present := 0
	if b.NodeID != "" {
		present++
	}
	if b.RoleID != "" {
		present++
	}
	if b.DeploymentID != "" {
		present++
	}
	if present != 2 {
		return nil, fmt.Errorf("Bind action requires 2 things to bind, got %d: %#v", present, b)
	}

	if b.NodeID != "" && b.RoleID != "" {
		return bindNodeRole(b)
	}
	if b.NodeID != "" && b.DeploymentID != "" {
		return moveNode(b)
	}
	if b.RoleID != "" && b.DeploymentID != "" {
		return bindDeploymentRole(b)
	}
	return nil, fmt.Errorf("Invalid binding: %#v", b)
}

func actionDelay(val interface{}) (Action, error) {
	array, ok := val.([]interface{})
	if !ok {
		return nil, errors.New("Delay needs nested actions")
	}
	if len(array) < 2 {
		return nil, errors.New("Delay needs at least one nested action and a duration")
	}

	delem, ok := array[0].(map[string]interface{})
	if !ok {
		return nil, errors.New("Delay needs a map as first element")
	}

	// First element must be a map with duration integer
	dobj, ok := delem["Duration"]
	if !ok {
		return nil, errors.New("Delay needs a map with duration as first element")
	}
	duration, ok := dobj.(float64)
	if !ok {
		return nil, errors.New("Duration must contain an integer")
	}

	actions := make([]Action, 0, 0)
	// The remaining elements must compile to actions
	for _, e := range array[1:] {
		element, ok := e.(map[string]interface{})
		if !ok {
			return nil, errors.New("Delay elements (after the first) should be actions")
		}
		subaction, err := ResolveAction(element)
		if err != nil {
			return nil, err
		}
		actions = append(actions, subaction)
	}

	return func(e *RunContext) error {
		ctx, err := e.Clone()
		if err != nil {
			return err
		}
		delay_actions(ctx, int(duration), actions)
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
		case "Delay":
			return actionDelay(v)
		case "Bind":
			return actionBind(v)
		case "Commit":
			return actionCommit(v)
		default:
			return nil, fmt.Errorf("Unknown action %s", t)
		}
	}
	return nil, fmt.Errorf("Cannot unmarshal action for #%v", a)
}
