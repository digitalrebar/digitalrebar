# DigitalRebar Event Rule Engine

This is the node rule engine daemon for Digital Rebar.

It listens for events, and for each event it recieves it
tests the event to see if it matches a set of user-provided rules, and
for each rule it matches it takes appropriate action.

## Building the tool:

* Download and install glide: https://glide.sh/
* Pull in the proper dependencies: glide install
* Build the tool: go build

## Command line options:
* backing: Backing store to use for RuleSets.  Permitted values are 'file' and 'consul' (default "file")
* dataloc: Path to store data at (default "/var/cache/rule-engine")
* debug: Whether to run in debug mode
* listen: Address for the API and the event listener to listen on.
* version: Print version and exit

## Interacting with the Rule Engine

### REST API

* GET rulesets/
  
  List all the rulesets the user making the request can see.
  
* GET rulesets/:name

  Fetch a ruleset by name.
  
* POST rulesets/

  Create a new ruleset.
  
* PUT rulesets/:name

  Update a ruleset by name
  
* DELETE rulesets/:name

  Delete a ruleset
  
### Capabilities

* RULESET_READ: Allows the ability to read rulesets in the tenant the user is a member of.
* RULESET_UPDATE: Allows the ability to update rulesets in the tenant that the user is a member of.  Includes the ability to delete a ruleset.

## Events

Events are emitted by the DigitalRebar core whenever something of interest happens.
It is the job of the Rule Engine to listen to those Events and take action based on the
Rulesets that the Rule Engine has loaded.  Think about how iptables works on Linux crossed
with a rule-based expert system, and you will not be too far off the mark.

### Event Definition

An incoming Event is a blob of JSON that unmaps to the following structure:

    type Event struct {
        Selector            EventSelector          `json:"selector"`
	      Event             *api.Event             `json:"event"`
	      Node              *api.Node              `json:"node"`
	      Role              *api.Role              `json:"role"`
	      NodeRole          *api.NodeRole          `json:"node_role"`
	      Deployment        *api.Deployment        `json:"deployment"`
	      DeploymentRole    *api.DeploymentRole    `json:"deployment_role"`
	      Network           *api.Network           `json:"network"`
	      NetworkAllocation *api.NetworkAllocation `json:"network_allocation"`
	      NetworkRange      *api.NetworkRange      `json:"network_range"`
	      NetworkRouter     *api.NetworkRouter     `json:"network_router"`
    }

Of those fields, only Selector and Event will be present in all Events.  The rest
will be present depending on which object inside of Digital Rebar the event was
created by:

* NodeRole: NodeRole, Node, Role, and Deployment will be filled.
* Node: Node and Deployment will be filled.
* DeploymentRole: Deployment, Role, and DeploymentRole will be filled.
* Deployment: only Deployment will be filled.
* Role: only Role will be filled.
* Network: only Network will be filled.
* NetworkAllocation: Network, Node, NetworkRange, and NetworkAllocation will be filled.
* NetworkRange: Network and NetworkRange will be filled.
* NetworkRouter: Network and NetworkRouter will be filled.

## Rulesets

Rulesets can be added to the rule engine by posting a YAML or JSON
blob to the appropriate API endpoint. Otherwise, rules will always be
returned from the rule engine in JSON format.

### An example ruleset:

    ---
    # Copyright (c) 2016, Rackn Inc.
    # Licensed under the terms of the Digital Rebar License.
    # See LICENSE.md at the top of this repository for more information.

    Name: 'test ruleset'
    Active: false
    Description: "The test ruleset, used to exercise the rule engine"
    Rules:
      - EventSelectors:
          - event: on_active
            obj_class: role
          - event: on_error
            obj_class: role
        Matchers:
          - Enabled: true
          - Eq: [ true ,true]
        Actions:
          - Script: |
              echo "Hello from {{.Evt.Node.UUID}} (AKA {{.Evt.Node.Name}})"
              echo "I an running because noderole {{.Evt.NodeRole.ID}} (for role {{.Evt.Role.Name}}) triggered {{.Evt.Selector.event}}"
          - Delay: 30
          - Script: |
              echo "This should be delayed 30 seconds: {{.Vars.eventType}}"
              env
          - Log: true
          - Stop: true
      - Name: 'test logging rule'
        EventSelectors:
          - event: on_milestone
        WantsAttribs:
          - disks
        Matchers:
          - Enabled: true
          - JSON: 
              Selector: ':root .Attribs .disks :has( .removable:val(false) )'
              SaveAs: 'harddisks'
          - Len:
              Var: harddisks
              SaveAs: driveCount
          - Gt: [ '$driveCount', 3 ]
        Actions:
          - Log: true
          - Script: |
              echo "System has {{.Vars.driveCount}} drives, which is more than the 3 we were looking for"
      - Name: 'test os install rule'
        Matchers:
          - GetAttrib:
              Node: "system-phantom.internal.local"
              Attrib: "disks"
              SaveAs: "phantom-drives"
          - JSON:
              Selector: ':root .Evt .role .name:val("rebar-managed-node")'
          - JSON:
              Selector: ':root .Evt .node .uuid'
              PickResults:
                nodeName: 0
        Actions:
          - Bind:
              NodeID: '$nodeName'
              RoleID: 'rebar-installed-node'
          - Commit:
              NodeID: '$nodeName'
          - Return: true

### RuleSet definition:

A RuleSet is composed of the following fields:
* Name: The name of the RuleSet.  Every RuleSet must be named, and ruleset names must be unique.
* Description: A description of a RuleSet.  Be as detailed as you want. 
* Active: Whether the RuleSet is active.  If the RuleSet is not active, then none of 
its rules will be used to process incoming Events.
* TenantID:  The tenant the ruleset is a member of.  It is populated by the rule engine on initial create., and cannot be changed by an update
* Username: The username that initially created the ruleset.
* Rules: The list of Rules for the RuleSet.

### Rule definition:

A Rule is composed of the following fields:

* Name: The name of the rule. It is only required if the rule is the target of a Jump or Call action.
Rule names in a RuleSet must be unique if they are present.

* EventSelectors: A list of selectors that describe the types of Events this Rule is interested in.
 A matched var, eventType, is created with the matched event.
* WantsAttribs: A list of DigitalRebar attribs or node attributes that the rule wants for Matchers 
to determine whether the Rule matches the Event, or that the Actions may need to perform their actions.

* Matchers: A list of Matchers that must match in order for the MatchActions to run.

* MatchActions: A list of Actions that will be run if all the Matchers match.

#### Matchers:
Currently, the rule engine knows about the following matchers:

* And: Takes a list of Matchers, and matches iff all of them match.

* Or: Takes a list of Matchers, and matches if any of them match.

* Not: Takes a single Matcher, and matches if it does not match.

* Enabled: Takes true or false, and matches if it was passed true.

* JSON: Extracts a value (or values) out of the JSON representation
of the RunContext.

  Takes a YAML object of the following format:

        ---
        Selector: ':root .Attribs .number_of_drives:val(5)'
        SaveAs: 'driveCount'
        PickResults:
          driveCount: 0

  The individual keys have the following meanings:

  * Selector: A JSONSelect selector fragment http://jsonselect.org/#overview.
  This selector will be matched against the JSON serialization of
  the RunContext that the rule is running in.  The Selector must
  match at least one thing in the RunContext unless PickResults
  was also specified, in which case it must match all of the
  variables that PickResults was asked to fill in.

  * SaveAs: The name of the variable to save whatever the Selector
  picked. SaveAs always saves the values it found in an array. If you
  want to get individual items, you should use PickResults instead.

  * PickResults: An object whose keys indicate variables to save and
  whose values indicate the index in the Selector results of the
  value that should be saved.  If PickResults and SaveAs refer to
  the same variable, PickResults wins.

* Script: Takes a string which will be compiled against the RunContext using
[text/template](https://golang.org/pkg/text/template/) into a shell script,  
and matches if the shell script exits with a zero status.  The shell script 
will be passed the following extra environment variables:

  * REBAR_ENDPOINT: The API endpoint that DigitalRebar lives on.

  * REBAR_KEY: The username:password for DigitalRebar

* Eq, Ne, Lt, Le, Gt, Ge: Takes a 2 element array of values.  
If the value is a string that begins with '$', the string will be interpreted
as the name of a variable.  If that variable has been set, its value will be
substituted.  To use a literal string beginning with $, escape it with
a \.  To begin with a literal \, escape it with \\.

* Len: Takes a YAML object that points to a variable to check the length of
and a variable name to save the length to.  If the variable to check
has a meaningful Length (i.e, it is an Array, a Map, or a String),
that length is saved and the matcher returns true, otherwise it
returns false.  The YAML object has the following format:

      ---
      Var: variable name to get the length of
      SaveAs: variable name to save the length to

* UUID: Takes a YAML object that specifies the thing to get the UUID of and
the name of the variable to save it in.  The YAML object has the following format:

      ---
      Node: string
      Role: string
      Deployment: string
      NodeRole: string
      DeploymentRole: string
      SaveAs: string

  Out of those fields, exactly one of Node, Role, Deployment,
  NodeRole, or DeploymentRole should be present.  The UUID of the
  object will be saved in the variable pointed to by SaveAs, otherwise
  the matcher will fail.

* GetAttrib: Takes a YAML object that specifies the attrib to get, the object to
get it from, and the variable to save the retrieved value to.  The
YAML object has the following format:

      ---
      Attrib: string
      Node: string
      Role: string
      Deployment: string
      NodeRole: string
      DeploymentRole: string
      SaveAs: string

  Out of those fields, Attrib must be filled with the name of the
  attrib to fetch, and exactly one of Node, Role, Deployment,
  NodeRole, or DeploymentRole should be present.  The value of the
  attrib will be saved in the variable referred to in SaveAs if it
  could be retrieved, otherwise the matcher will fail.

#### Actions:
Currently, the rule engine knows about the following actions:

* Log: Emit a hardcoded logging message.

* Script: Takes a string which will be compiled against the RunContext using
[text/template](https://golang.org/pkg/text/template/) into a shell script,
which will be passed the following extra environment variables:

  * REBAR_ENDPOINT: The API endpoint that DigitalRebar lives on.

  * REBAR_KEY :The username:password for DigitalRebar

* Delay: Takes an integer representing the number of seconds the action should sleep for.

* Bind: Takes YAML object with the following format:

      ---
      NodeID: string
      DeploymentID: string
      RoleID: string
      SaveAs: string

  Exactly 2 of the ID fields must be filled, and which two determine
  what action Bind will take:

  * NodeID and RoleID: a Role will be bound to a Node

  * DeploymentID and NodeID: a Node will be moved to the Deployment

  * RoleID and DeploymentID: a Role will be bound to the Deployment

  If SaveAs is set, the resultant new object's unique identifier (if one was created) will be saved to the referenced variable.

* SetAttrib: Takes a YAML object with the following format:

      ---
      Attrib: string
      NodeID: string
      DeploymentID: string
      NodeRoleID: string
      DeploymentRoleID: string
      SaveAs: string
      Value: anything

  Exactly one of the Node, Deployment, NodeRole, or DeploymentRole must
  be populated with something that uniquely identifies the object type
  in question.  The Attrib field must be the name of the attrib to set
  (which must be valid for the object in question), and the Value field
  must be the value to set it to.  Note that setting the attrib value
  does not commit the object, you must use a Commit action afterwards to
  commit the value.

* Commit: Takes a YAML object with the following format:

      ---
      NodeID: string
      DeploymentID: string
      DeploymentRoleID: string
      NodeRoleID: string

  Exactly one of the fields must be filled, and that thing will be committed.

* Retry: Takes a YAML object with the following format:

      ---
      NodeRoleID: string

  The specified NodeRole is retried.  The normal usage will be to get the node role
  uuid from the event.  This is a string.

* Stop: Takes a boolean argument, which is ignored. 
Stop tells the RunContext to stop processing rules after finishing with this one.
Stop, Jump, Call, and Return are mutually exclusive -- only one of these can
be present on any given Rule.

* Jump: Takes a string argument, which must be the name of the Rule to jump
to when this Rule finishes processing successfully.  The named Rule to jump to
must appear after the current Rule in the Rules list for this RuleSet.  This removes
the possibility of rule processing entering an infinite loop.

* Call: Behaves like (and has the same restrictions as) Jump, except that the
RunContext will return control to the Rule defined after this one if it encounters
a Return action.

* Return: Takes a boolean argument, which is ignored.  Upon successful completion of the
Rule actions, tells the RunContext to return to the rule after the most recent Call.  If
no Call has been made, Return functions like Stop.

## Flow of Events through RuleSets

Upon recieving an Event, the following steps happen:

1. The Rule Engine matches the Selector against the EventSelectors of the individual Rules.
2. A RunContext is created for each RuleSet that contains a Rule that matches the EventSelector.
3. Each RunContext is handed a RunList of Rules in its RuleSet to start execution at.
4. Each RunContext starts executing Rules starting at the first Rule in its RunList.
Each RunContext performs the next steps independently.
5. Set the current Rule to the current entry in the RunList, then advance
the RunList to the next entry. If there is no current entry, then stop.
6. Execute the Matchers for the current Rule in order.
If any do not match, move to the next Rule in the RuleSet and go to step 6.
7. Execute the Actions in order. If any fail, go to step 5.
7. If any control-flow Actions (Stop, Jump, Call, or Return) were present on the Rule,
honor them, otherwise continue to the next Rule in the RuleSet.  Go to step 6.
8. If the RunContext runs out of things to do (due to a Stop or Return action,
or by running out of Rules in the RuleSet), go to step 5.
