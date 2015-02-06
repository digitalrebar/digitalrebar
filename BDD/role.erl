% Copyright 2013, Dell 
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
-module(role).
-export([step/2, json/3, validate/1, inspector/0, g/1]).
-include("bdd.hrl").

% Commont Routine
% Provide Feature scoped strings to DRY the code
g(Item) ->
  case Item of
    path  -> "/api/v2/roles";
    atom  -> bdd_role1;
    name  -> "bdd_role";
    _     -> crowbar:g(Item)
  end.
  

% Common Routine
% Makes sure that the JSON conforms to expectations (only tests deltas)
validate(JSON) when is_record(JSON, obj) ->
  J = JSON#obj.data,
  R =[JSON#obj.type == "role",
      bdd_utils:is_a(J, length, 21),
      bdd_utils:is_a(J, boolean, library),
      bdd_utils:is_a(J, boolean, powersave),
      bdd_utils:is_a(J, boolean, service),
      bdd_utils:is_a(J, boolean, implicit),
      bdd_utils:is_a(J, boolean, bootstrap),
      bdd_utils:is_a(J, boolean, milestone),
      bdd_utils:is_a(J, boolean, discovery),
      bdd_utils:is_a(J, boolean, destructive),
      bdd_utils:is_a(J, boolean, abstract),
      bdd_utils:is_a(J, boolean, cluster),
      bdd_utils:is_a(J, boolean, destructive),
      bdd_utils:is_a(J, string, jig_name),
      bdd_utils:is_a(J, int,    cohort),
      bdd_utils:is_a(J, dbid,   barclamp_id),
      bdd_utils:is_a(J, string, template),
      bdd_utils:is_a(J, string, conflicts),
      bdd_utils:is_a(J, string, provides),
      crowbar_rest:validate(J)],
  bdd_utils:assert(R).

% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector() -> 
  bdd_restrat:inspector(role).  % shared inspector works here, but may not always

% Common Routine
% Creates JSON used for POST/PUT requests
json(Name, Description, Order) ->
  json:output([{"name",Name},{"description", Description}, {"order", Order}]).

     
% Common Routines
step(_Global, {step_given, {_Scenario, _N}, ["I set the",role, Role, "property", Property, "to", Value]}) -> 
  Path = eurl:path([g(path), Role, "template", Property, Value]),
  J = "{}",
  bdd_utils:log(debug, role, step, "~p PUT ~p", [Path, J]),
  % now update 
  Result = eurl:put_post(Path, J, put),
  O = bdd_restrat:get_object(Result),
  [Result, O];

step(_Global, {step_setup, _N, _}) -> 
  % create node(s) for tests
  JSON = json(g(name), g(description), 100),
  bdd_crud:create(g(path), JSON, g(atom));

step(_Global, {step_teardown, _N, _}) -> 
  % find the node from setup and remove it
  bdd_crud:delete(g(atom)).  
