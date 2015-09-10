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
-module(attrib).
-export([step/2, json/3, validate/1, inspector/0, g/1]).
-include("bdd.hrl").

% Commont Routine
% Provide Feature scoped strings to DRY the code
g(Item) ->
  case Item of
    path  -> "api/v2/attribs";
    name  -> "bddattribute";
    atom  -> attrib1;
    map   -> 'bdd/rocks';
    value -> 'bravo';
    _ -> rebar:g(Item)
  end.
  
% Common Routine
% Makes sure that the JSON conforms to expectations (only tests deltas)
validate(JSON) when is_record(JSON, obj) ->
  J = JSON#obj.data,
  R =[JSON#obj.type == "attrib",
      bdd_utils:is_a(J, string, map),
      bdd_utils:is_a(J, dbid, role_id), 
      bdd_utils:is_a(J, dbid, barclamp_id), 
      bdd_utils:is_a(J, boolean, writable), 
      bdd_utils:is_a(J, string, schema), 
      bdd_utils:is_a(J, string, ui_renderer), 
      bdd_utils:is_a(J, length, 13),
      rebar_rest:validate(J)],
  bdd_utils:assert(R).
 
% Common Routine
% Creates JSON used for POST/PUT requests
json(Name, Description, Order) -> 
  rebar:json([{name, Name}, {description, Description}, {order, Order}]).

% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector() -> 
  bdd_restrat:inspector(attrib).  % shared inspector works here, but may not always

set_attrib(Type, Name, Attrib, Value) ->
  bdd_utils:log(debug, attrib, set_attrib, " Attribu Update ~p on ~p to ~p", [Type, Name, Value]),
  Path = eurl:path([apply(Type, g, [path]), Name, "attribs", Attrib]),
  % this ASSUMES that the Value is valid JSON
  JSON = rebar:json([{value, Value}]),
  bdd_utils:log(debug, attrib, set_attrib, "~p PUTS ~p", [Path, JSON]),
  % now update 
  Result = eurl:put_post(Path, JSON, put),
  O = bdd_restrat:get_object(Result),
  [Result, O].

% Common Routine

step(_Global, {step_given, {Scenario, _N}, ["REST creates the", attrib, Name, "with map",Map]}) -> 
  JSON = rebar:json([{name, Name}, {description, g(description)}, {barclamp, 'test'}, {order, g(order)}, {writable, true}, {map, Map}]),
  bdd_restrat:create(g(path), JSON, attrib, Scenario);

step(_Global, {step_given, {Scenario, _N}, ["REST creates the", attrib, Name, "with barclamp",Barclamp]}) -> 
  JSON = rebar:json([{name, Name}, {description, g(description)}, {barclamp, Barclamp}, {order, g(order)}, {writable, true}]),
  bdd_restrat:create(g(path), JSON, attrib, Scenario);

step(_Given, {step_given, {_Scenario, _N}, ["REST sets the node", Attrib, "on", Node, "to", Value]}) -> 
  step(_Given, {step_when, {_Scenario, _N}, ["REST sets the node", Attrib, "on", Node, "to", Value]});

step(_Given, {step_when, {_Scenario, _N}, ["REST sets the node", Attrib, "on", Node, "to", Value]}) -> 
  set_attrib(node, Node, Attrib, Value);

step(_Given, {step_when, {_Scenario, _N}, ["REST sets the role", Attrib, "on", Role, "to", Value]}) -> 
  set_attrib(role, Role, Attrib, Value);

step(_Global, {step_setup, _N, _}) -> true;

step(_Global, {step_teardown, _N, _}) -> true.
