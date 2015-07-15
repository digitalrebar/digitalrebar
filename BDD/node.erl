% Copyright 2014, Dell 
% 
% Licensed under the Apache License, Version 2.0 (the "License"); 
% you may not use this file except in compliance with the License. 
% You may obtain a copy of the License at 
% 
%  http://www.apache.org/licenses/LICENSE-2.0 
% 
% Unless required by applicable law or agreed to in writing, software 
% distributed under the License is distributed on an "AS IS" BASIS, 
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
% See the License for the specific language governing permissions and 
% limitations under the License. 
% 
% 
-module(node).
-export([step/2, json/3, add_node/4, add_node/3, create_node/3, validate/1, update/2, bind/2, bind/3, alive/1, commit/1, inspector/0, g/1]).
-include("bdd.hrl").


% Commont Routine
% Provide Feature scoped strings to DRY the code
g(Item) ->
  case Item of
    path -> "api/v2/nodes";
    status_path -> "/api/status/nodes";
    name -> "bdd1.example.com";
    bootenv -> "local";
    atom -> node1;
    role -> "crowbar-managed-node";
    deployment -> "system";
    _ -> crowbar:g(Item)
  end.

% Common Routine
% Makes sure that the JSON conforms to expectations (only tests deltas)
validate(JSON) when is_record(JSON, obj) ->
  J = JSON#obj.data,
  R =[JSON#obj.type == "node",
      bdd_utils:is_a(J, length, 18),
      bdd_utils:is_a(J, boolean, alive),
      bdd_utils:is_a(J, boolean, system),
      bdd_utils:is_a(J, boolean, available),
      bdd_utils:is_a(J, boolean, allocated),
      bdd_utils:is_a(J, boolean, admin),
      bdd_utils:is_a(J, string, bootenv),
      bdd_utils:is_a(J, string, discovery),
      bdd_utils:is_a(J, string, hint),
      bdd_utils:is_a(J, dbid, deployment_id),
      bdd_utils:is_a(J, dbid, target_role_id),
      bdd_utils:is_a(J, integer, order),
      crowbar_rest:validate(J)],
  bdd_utils:assert(R).

% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector() -> 
  bdd_restrat:inspector(node).  % shared inspector works here, but may not always

% Common Routine
% Creates JSON used for POST/PUT requests
json(Name, Description, Order) -> crowbar:json([{name, Name}, {description, Description}, {order, Order}, {alive, "true"}, {bootenv, node:g(bootenv)}]).

% only CREATES, you still have to bind, commit and turn on 
create_node(Name, Params, Atom) ->
  Path = bdd_restrat:alias(node, g, [path]),
  Obj = bdd_crud:read_obj(Path,Name),
  case Obj#obj.id of
    "-1" -> 
          P = lists:append([{name, Name}, {alive, true}, {bootenv, node:g(bootenv)}], Params),
          JSON = crowbar:json(P),
          bdd_utils:log(trace, node, create_node, "Creating Node ~p on ~p with ~p", [Name, Path, JSON]),
          [_R, O] = bdd_crud:create(Path, JSON, Atom),
          bdd_utils:log(debug, node, create_node, "Node ~p created (id ~p)", [Name, O#obj.id]),
          O;
    _  -> bdd_utils:config_set(Atom, Obj),
          bdd_utils:log(debug, node, create_node, "Node ~p already exists (id ~p)", [Name, Obj#obj.id]),
          Obj
  end.

% does full cycle create
add_node(Name, Params, Atom) -> add_node(Name, g(role), Params, Atom).
add_node(Name, Role, Params, Atom) ->
  O = create_node(Name, Params, Atom),
  bind(O#obj.id, Role),
  % add os choices for admin node
  case Role of
    "crowbar-admin-node" -> node_role:available_os(O#obj.id, ["ubuntu-12.04","centos-6.6"]);
    _ -> noop
  end,
  % complete deploy
  commit(O#obj.id),
  alive(O#obj.id),
  O.

alive(Node) -> update(Node, [{"alive","true"}]).

commit(Node) ->
  Path = bdd_restrat:alias(node, g, [path]),
  R = bdd_crud:update(eurl:path([Path, Node, "commit"]), ""),
  bdd_utils:log(debug, node, update, "Node ~p Commit Returned ~p", [Node, R]),
  R.

update(Node, KVP) ->
  Path = bdd_restrat:alias(node, g, [path]),
  R = bdd_crud:update(eurl:path([Path, Node]), json:output(KVP)),
  bdd_utils:log(debug, node, update, "Node ~p Update Set ~p Returned ~p", [Node, KVP, R]),
  R.
     
% local helpers
bind(Node, Role) -> bind(Node, Role, "system").
bind(Node, Role, Deployment) -> node_role:bind(Node, Role, Deployment).

% Common Routines

step(_Given, {step_given, {_Scenario, _N}, ["REST sets the",node,Node,Field,"state to be",Value]}) -> 
  step(_Given, {step_when, {_Scenario, _N}, ["REST sets the",node,Node,Field,"state to be",Value]});
step(_Given, {step_when, {_Scenario, _N}, ["REST sets the",node,Node,Field,"state to be",Value]}) -> 
  % this could be done genericly in bdd_restrat if we get it working!!
  URI = eurl:path([g(path), Node]),
  JSON = crowbar:json([{Field, Value}]),
  bdd_crud:update(URI, JSON);

step(_Global, {step_given, {ScenarioID, _N}, ["REST creates and commits the",node,Name]}) ->
  add_node(Name, [], ScenarioID);

step(_Global, {step_setup, _N, _}) -> 
  % create node(s) for tests
  Node = json(g(name), g(description), 100),
  bdd_crud:create(g(path), Node, g(atom)),
  crowbar:step([], {foo, {0,0}, ["process", "delayed","returns", "delayed_job.([0..9])"]});

step(_Global, {step_teardown, _N, _}) -> 
  % find the node from setup and remove it
  crowbar:step([], {step_given, {0, 0}, ["there are no pending Crowbar runs for",node,g(name)]}), 
  bdd_crud:delete(g(atom)).  
