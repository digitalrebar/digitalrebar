% Copyright 2012, Dell 
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
-module(jig).
-export([step/2, json/3, json/4, validate/1, inspector/0, g/1]).
-include("bdd.hrl").

% Commont Routine
% Provide Feature scoped strings to DRY the code
g(Item) ->
  case Item of
    path -> "api/v2/jigs";
    name -> "bddjig";
    atom -> jig1;
    type -> "BarclampTest::Jig";
    node_atom -> "global-node.testing.com";
    _ -> rebar:g(Item)
  end.

% Common Routine
% Makes sure that the JSON conforms to expectations (only tests deltas)
validate(JSON) when is_record(JSON, obj) ->
  J = JSON#obj.data,
  R =[JSON#obj.type == "jig",
      bdd_utils:is_a(J, length, 11),
      bdd_utils:is_a(J, string, server),
      bdd_utils:is_a(J, string, client_role_name),
      bdd_utils:is_a(J, string, client_name),
      bdd_utils:is_a(J, string, key),
      bdd_utils:is_a(J, boolean, active),
      rebar_rest:validate(J)],
  bdd_utils:assert(R, debug);
validate(JSON) -> 
  bdd_utils:log(error, barclamp, validate, "requires #obj record. Got ~p", [JSON]), 
  false.

% Common Routine
% Creates JSON used for POST/PUT requests
json(Name, Description, Order)        -> json(Name, Description, g(type), Order).
json(Name, Description, Type, Order)  ->
  json:output([{"name",Name},{"description", Description}, {"type", Type}, {"order", Order}]).

% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector() -> 
  bdd_restrat:inspector(jig).  % shared inspector works here, but may not always

step(_Global, {step_given, {Scenario, _N}, ["there is a jig",Jig,"of type", Type]}) -> 
  JSON = json(Jig, g(description), Type, 200),
  bdd_crud:create(g(path), JSON, Scenario, Jig);

step(_Global, {step_setup, _N, _}) -> 
  % create Jig entry
  Jig = json(g(name), g(description), g(type), 100),
  bdd_crud:create(g(path), Jig, g(atom));

step(_Global, {step_teardown, _N, _}) -> 
  % remove jig entry
  bdd_crud:delete(g(atom)).
