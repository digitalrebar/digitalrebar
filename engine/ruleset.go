package engine

import (
	"fmt"
	"log"

	"github.com/digitalrebar/go-common/event"
	"github.com/pborman/uuid"
)

// Rule defines a set of Actions to perform if the Matchers match.
//
// Fields of interest:
//
// EventSelectors: a slice of EventSelector that determines which
// incoming Events this Rule should be triggered for. A Rule can be
// triggered by multiple Events, and multiple Rules in a RuleSet can
// be triggered by the same Event. In that case, the Rules will be run
// in the order in which they were defined. Rules do not have to have
// EventSelectors, but only rules with EventSelectors will be invoked
// directly in response to an incoming Event.
//
// Matchers: A list of objects that the Engine uses to determine
// whether this Rule should take action in response to an incoming
// Event.  Each object ahould have a single key with the name of the
// Matcher, and a value specific to that Matcher. Currently, the
// Engine knows about the following Matchers:
//
// "And" and "Or", which expect their values to be arrays of key-value
// pairs that can be compiled by ResolveMatch.  "And" will match if
// all of its matchers match, and "Or" will match if any of its
// matchers match.  Both operators will only execute enough of their
// matchers to determine if the overall matcher will succeed (for
// "Or"), or fail (for "And").  If no matchers are passed, "And" will
// signal that it matched, and "Or" will signal that it did not match.
//
//
// "Not", which expects its value to be a single key-value pair that
// can be compiled by ResolveMatcher.  "Not" will invert the return
// value of its matcher.
//
//
// "Enabled", which expects its value to be a bool.  It will return
// the value of that boolean without change, and (as the name
// suggests) is expected to enable and disable rules for testing
// purposes.
//
//
// "JSON", which expects a struct with the following format:
//   struct {
//		Selector    string
//		SaveAs      string
//		PickResults map[string]float64
//	 }
//
// "Selector" is a string containing a JSON Selector matching the
// specification at http://jsonselect.org/#overview.  The selector
// will return an array of results containing each individual item
// from the RunContext that was passed into the Matcher.
//
// "SaveAs" is an optional variable name to save the results the
// selector returned.  The entire array of results will be saved, even
// if it is only one element long.
//
// "PickResults" is an optional map of variables names to selector
// indexes.  After the selector returns its results and they have been
// optionally saved to the variable pointed to by SaveAs, PickResults
// will save data from the selector indexes to the matching variable
// names.  If the selector results do not contain a requested index,
// then the match will fail with an error.  You should construct
// Selector and PickResults with that in mind.  In the absence of
// PickResults, a JSON match fails if nothing was selected or the
// Selector was invalid, otherwise it passes.
//
//
// "Script", which expects its value to be a string that will be
// parsed using text.Template.  The result of that parsing should be a
// valid bash script.  When the Matcher is ran, the parsed script will
// be compiled using the passed RunContext.  If the compilation fails,
// the match will fail with an error, so be sure to write your scripts
// with that in mind.  Otherwise, the matcher will pass if the script
// exits with a zero and fail otherwise.
//
//
// "Eq", "Ne", "Lt", "Le", "Gt", "Ge", which are basic 2-item
// comparison functions that expect a 2 element array of values.  If
// the value is a string that begins with '$', the string will be
// interpreted as the name of a variable.  If that variable has been
// set, its value will be substituted.  To use a literal string
// beginning with $, escape it with a \.  To begin with a literal \,
// escape it with \\.
//
//
// "Len", which expects its value to be a struct with the following
// format:
//
//    struct {
//        Var string
//        SaveAs string
//    }
//
// If the variable saved in Var is something with a length (an array,
// a map, or a string), the matcher will return true and save the
// length of the variable in the variable named by SaveAs, otherwise
// it will return false.
//
//
// "UUID", which expects its value to be a struct matching the
// following format:
//
//    struct {
//        Node string
//        Role string
//        Deployment string
//        NodeRole string
//        DeploymentRole string
//        SaveAs string
//    }
//
// Out if those fields, exactly one of Node, Role, Deployment,
// NodeRole, and DeploymentRole should be filled, and their values must
// resolve to an existing object of their type, otherwise the matcher will not match.
//If there is such an object, its UUID will be saved in SaveAs.
//
//
// "GetAttrib", which expects its value to be a struct with the following format:
//
//    struct {
//        Attrib string
//        Node string
//        Role string
//        Deployment string
//        NodeRole string
//        DeploymentRole string
//        SaveAs string
//    }
//
//
// Out if those fields, exactly one of Node, Role, Deployment,
// NodeRole, and DeploymentRole should be filled, and their values
// must resolve to an existing object of their type, otherwise the
// matcher will not match.  Attrib must also be filled with the name
// of the Attrib to fetch.  If the vlue of the attrib can be fetched
// for the object, it will be saved in the variable named by SaveAs
//
//
// Actions:  a list of objects that define what this Rule should do if it Matches.
// Each object should have a single key/value pair, the key of which is the name
// of the Action to perform and the value of which is specific to each Action.
// The Engine currently understands the following Actions:
//
// "Log", which causes the engine to emit a hard-coded logging message
// to stderr.  This is mainly for testing purposes so far, but we may
// repurpose it to emit something useful later.
//
//
// "Script", which expects its value to be a string that will be
// parsed using text.Template.  The result of that parsing should be a
// valid bash script.  When the Action is ran, the parsed script will
// be compiled using the passed RunContext.  If the compilation fails,
// or the resultant script executes with a non-zero exit status, the
// Action will fail, otherwise it will pass.
//
//
// "Delay", which expects its value to be the number of seconds to
// pause for before performing the next Action.
//
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
//
// "Retry", which causes a node role to be retried.
//    struct {
//        NodeRoleID string
//    }
// This causes the node role to be retried.
//
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
//
//
// "Commit", which expects its value to be a struct with the following format:
//
//    struct {
//        NodeID: string
//        DeploymentID: string
//        DeploymentRoleID: string
//        NodeRoleID: string
//    }
//
// Exactly one of those fields must contain the name or UUID of the thing to be committed,
// and the thing will be committed.
//
//
// "Stop", which takes a boolean value which is ignored.  Stop tells
// the RunContext to stop processing Rules for this event,
// effeectively making the current Rule the last in the chain of Rules
// the RunContext would process for the list of Rules the RunContext
// was created to process.
//
//
// "Jump", which takes a string value naming the Rule in the RuleSet
// to jump to after the current Rule has finished processing.  Jumps
// must be forward -- the rule you wish to jump to must appear in the
// list of Rules for the RuleSet after the currently executing Rule.
//
//
// "Call", which behaves like (and has the same restrictions as) Jump,
// except that the RunContext will return control to the Rule defined
// after the currently executing one if it encounters a Return action.
//
//
// "Return" behaves like Stop if there has been no Call action,
// otherwise it returns control to the Rule immediately after the one
// that had the most recent Call action.
type Rule struct {
	// Name of the Rule.  Not required, but unnnamed Rules can only
	// be invoked by direct triggers from an incoming Event.
	Name string
	// EventSelectors is a slice of EventSelector
	// that determines which incoming Events will trigger
	// this Rule. See the overall Rule documentation for more details.
	EventSelectors []event.Selector
	// The Attributes that the rule needs to perform its job.  The Engine
	// will fetch these Attributes from the Digital Rebar core prior
	// to running the Rule.
	WantsAttribs []string
	// The matchers must all match for the Actions to be
	// performed.  If any of the matchers do not match, the
	// Actions will not be performed and the Rule will be
	// considered to not match the Event.  If any Matcher fails,
	// it will terminate processing of the RunContext that is
	// interpreting Rules for the Event.  See the detailed rule
	// description for the Matchers the Engine currently supports.
	Matchers []map[string]interface{}
	// If all the Matchers match, the Actions for the Step will be
	// performed.  If any of the Actions fail, then the Rule fails
	// overall, and the RunContext that is running Rules against
	// the Event will be terminated.  See the detailed rule
	// description for the Actions the Engine currently supports.
	Actions  []map[string]interface{}
	matchers []matcher // compiled Matchers
	actions  []action  // compiled Actions
}

// Match runs the step Matchers.  If all the Matchers match, then the
// step is considered an overall Match.
func (r *Rule) match(c *RunContext) (bool, error) {
	return matchAnd(r.matchers...)(c)
}

// RunActions runs the step MatchActions in order.  If any of them
// return an error, no further MatchActions will be run.
func (r *Rule) run(c *RunContext) error {
	for _, action := range r.actions {
		if err := action(c); err != nil {
			return err
		}
	}
	return nil
}

// RuleSet defines a discrete bundle of functionality
// that the Classifier operates on. Multiple Rulesets can
// be loaded into the Classifier, and they will all be processed independently.
type RuleSet struct {
	// Rulesets that are not Active will not recieve Events, and therefore
	// will not do anything.
	Active bool
	// Globally unique name of the RuleSet
	Name string
	// Human-readable description of what this RuleSet is for.
	Description string
	// Version of the RuleSet.  Used to detect conflicting RuleSet changes.
	// API calls must provide this on Update and Delete calls.
	Version uuid.UUID
	// List of Rules that this RuleSet encapsulates.  The order of the Rules
	// is important, because Call and Jump actions can only call named Rules
	// that appear earlier in the Rule list.  This is important because we
	// need to guarantee that the processing of any given Event will eventually
	// terminate, and one of the better ways to do that is to ensure that we cannot
	// have loops in rule processing.  This restriction may be relaxed in the future.
	Rules      []Rule
	TenantID   int64
	Username   string
	engine     *Engine
	namedRules map[string]int
}

func (rs *RuleSet) compile(e *Engine) error {
	rs.namedRules = map[string]int{}
	for i := range rs.Rules {
		rule := &rs.Rules[i]
		// Make sure we don't have conflicting rule names
		if rule.Name != "" {
			conflict, ok := rs.namedRules[rule.Name]
			if ok {
				return fmt.Errorf("Ruleset %s: Rule %d (name %s) has the same name as Rule %d", rs.Name, i, rule.Name, conflict)
			}
			log.Printf("Adding named rule '%s'", rule.Name)
			rs.namedRules[rule.Name] = i
		}
	}
	for i := range rs.Rules {
		rule := &rs.Rules[i]
		rule.matchers = make([]matcher, len(rule.Matchers))
		rule.actions = make([]action, len(rule.Actions))
		for l, m := range rule.Matchers {
			log.Printf("Compiling ruleset %s rule %d matcher %d", rs.Name, i, l)
			match, err := resolveMatcher(e, rule, m)
			if err != nil {
				return err
			}
			rule.matchers[l] = match
		}
		for l, a := range rule.Actions {
			log.Printf("Compiling ruleset %s rule %d action %d", rs.Name, i, l)
			action, err := resolveAction(e, rs, i, a)
			if err != nil {
				return err
			}
			rule.actions[l] = action
		}
	}
	return nil
}
