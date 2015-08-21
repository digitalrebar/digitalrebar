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
-module(node_role).
-export([step/2, json/3, validate/1, inspector/0, g/1, bind/2, bind/3]).
-export([attrib_set/3, available_os/2]).
-include("bdd.hrl").

% Commont Routine
% Provide Feature scoped strings to DRY the code
g(Item) ->
  case Item of
    path  -> "/api/v2/node_roles";
    atom  -> bdd_node_role1;
    name  -> "bdd_node_role";
    _     -> crowbar:g(Item)
  end.
  

% Common Routine
% Makes sure that the JSON conforms to expectations (only tests deltas)
validate(JSON) when is_record(JSON, obj) ->
  J = JSON#obj.data,
  R =[JSON#obj.type == "node_role",
      bdd_utils:is_a(J, length, 14),
      bdd_utils:is_a(J, dbid, node_id),
      bdd_utils:is_a(J, dbid, role_id),
      bdd_utils:is_a(J, dbid, deployment_id),
      bdd_utils:is_a(J, integer, state),
      bdd_utils:is_a(J, integer, cohort),
      bdd_utils:is_a(J, integer, run_count),
      bdd_utils:is_a(J, string, runlog),
      bdd_utils:is_a(J, string, status),
      bdd_utils:is_a(J, boolean, available),
      bdd_utils:is_a(J, boolean, node_error),
      crowbar_rest:validate_core(J)],
  bdd_utils:assert(R).

% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector() -> 
  bdd_restrat:inspector(node_role).  % shared inspector works here, but may not always

% Common Routine
% Creates JSON used for POST/PUT requests
json(Name, Description, Order) ->
  json:output([{"name",Name},{"description", Description}, {"order", Order}, {"role","test-client"}, {"node",node:g(node_atom)}, {"snapshot", "system"}]).

bind(Node, Role) -> bind(Node, Role, "system").
bind(Node, Role, Deployment) ->
  Path = bdd_restrat:alias(node_role, g, [path]),
  JSON = json:output([{"node", Node}, {"role", Role}, {"deployment", Deployment}]),
  bdd_crud:create(Path, JSON).

% specialized function

available_os(AdminNode, OS) -> 
  attrib_set(AdminNode, "provisioner-base-images", json:output([{data,[{crowbar,[{provisioner, [{server, [{available_oses, [{K, true} || K <- OS]}]}]}]}]}])).

attrib_set(Node, Attrib, ValueJSON) ->
  bdd_utils:log(debug, node_role, attrib_set, "Calling with node ~p attrib ~p set ~p", [Node, Attrib, ValueJSON]),
  URI = eurl:path([node:g(path), Node, "node_roles", Attrib]),
  eurl:put(URI, ValueJSON).

% Common Routines

step(_Global, {step_setup, _N, _}) -> 
  % create node(s) for tests
  JSON = json(g(name), g(description), 100),
  bdd_crud:create(g(path), JSON, g(atom));

step(_Global, {step_teardown, _N, _}) -> 
  % find the node from setup and remove it
  bdd_crud:delete(g(atom)).  
