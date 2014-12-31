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
-module(deployment).
-export([step/2, json/3, validate/1, inspector/1, g/1, create/3]).
-export([attrib_set/3]).
-include("bdd.hrl").

% Commont Routine
% Provide Feature scoped strings to DRY the code
g(Item) ->
  case Item of
    path -> "api/v2/deployments";
    system -> "system";
    resource -> "deployments";
    d_name -> "bravo_delta";
    d_atom -> deployment_bdd;
    _ -> crowbar:g(Item)
  end.

% Common Routine
% Makes sure that the JSON conforms to expectations (only tests deltas)
validate(JSON) when is_record(JSON, obj) ->
  J = JSON#obj.data,
  R =[JSON#obj.type == "deployment",
      bdd_utils:is_a(J, length, 8),
      bdd_utils:is_a(J, boolean, system),
      bdd_utils:is_a(J, dbid, parent_id),
      bdd_utils:is_a(J, integer, state),
      bdd_utils:is_a(J, string, description),
      crowbar_rest:validate(J)],
  bdd_utils:assert(R);
validate(JSON) -> 
  bdd_utils:log(error, deployment, validate, "requires #obj record. Got ~p", [JSON]), 
  false.

create(ID, Name, Extras) ->
  % for now, we are ignoring the extras
  JSON = json(Name, 
              proplists:get_value(description, Extras, g(description)), 
              proplists:get_value(order, Extras, g(order))),
  bdd_restrat:create(ID, node, g(path), Name, JSON).
  
% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector(Deployment) -> 
  bdd_restrat:inspector(Deployment, deployment).  % shared inspector works here, but may not always

% Common Routine
% Creates JSON used for POST/PUT requests
json(Name, Description, Order) ->
  crowbar:json([{name, Name}, {description, Description}, {order, Order}]).

% specialized function

attrib_set(Deployment, Attrib, ValueJSON) ->
  URI = eurl:path([g(path), Deployment, "attribs", Attrib]),
  eurl:put(URI, ValueJSON).

% steps

step(_Global, {step_setup, _N, _}) -> 
  % create DEPLOYMENTS(s) for tests
  Deploy = json(g(d_name), g(description), 100),
  bdd_crud:create(g(path), Deploy, g(d_atom));

step(_Global, {step_teardown, _N, _}) -> 
  % find the node from setup and remove it
  bdd_crud:delete(g(d_atom));

step(_Result, {_Type, _N, ["END OF CONFIG"]}) ->
  false.
