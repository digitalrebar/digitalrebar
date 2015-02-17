% Copyright 2015, RackN, Rob Hirschfeld
% 
% Licensed under the Apache License, Version 2.0 (the "License"); 
% you may not use this file except in compliance with the License. 
% You may obtain a copy of the License at 
% 
%  http://www.apache.org/licensefs/LICENSE-2.0 
% 
% Unless required by applicable law or agreed to in writing, software 
% distributed under the License is distributed on an "AS IS" BASIS, 
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
% See the License for the specific language governing permissions and 
% limitations under the License. 
% 
% 
-module(hammer).
-export([step/2, json/1, json/3, validate/1, inspector/0, g/1]).
-include("bdd.hrl").

% Commont Routine
% Provide Feature scoped strings to DRY the code
g(Item) ->
  case Item of
    path -> "api/v2/hammers";
    name -> "bddhammer";
    atom -> hammer1;
    type -> "BarclampTest::Hammer";
    node_atom -> "global-node.testing.com";
    _ -> crowbar:g(Item)
  end.

% Common Routine
% Makes sure that the JSON conforms to expectations (only tests deltas)
validate(JSON) when is_record(JSON, obj) ->
  J = JSON#obj.data,
  R =[hammer_type(JSON#obj.type),
      bdd_utils:is_a(J, length, 9),
      bdd_utils:is_a(J, int, node_id),
      bdd_utils:is_a(J, string, name),
      bdd_utils:is_a(J, str, authenticator),
      bdd_utils:is_a(J, int, priority),
      bdd_utils:is_a(J, str, username),
      bdd_utils:is_a(J, integer, available_hammer_id),
      bdd_utils:is_a(J, array, actions),
      bdd_utils:is_a(J, integer, id)],
  bdd_utils:assert(R, debug);

validate(JSON) -> 
  bdd_utils:log(error, hammer, validate, "requires #obj record. Got ~p", [JSON]), 
  false.

hammer_type(Type) ->
  bdd_utils:log(debug, hammer, hammer_type, "checking #obj.type ~p", [Type]),   
  try re:run(Type, "(.*)hammer") of
    nomatch        -> false;
    {match, _} -> true
  catch
    _: _ -> false
  end.

% Common Routine
% Creates JSON used for POST/PUT requests
json(Name, _, _)  -> json(Name).
json(Name)        -> json:output([{"name",Name}]).

% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector() -> 
  bdd_restrat:inspector(hammer).  % shared inspector works here, but may not always

step(_Global, {step_given, {Scenario, _N}, ["there is a hammer",Hammer]}) -> 
  JSON = json(Hammer),
  bdd_crud:create(g(path), JSON, Scenario, Hammer);

step(_Global, {step_setup, _N, _}) -> 
  % create Hammer entry
  Hammer = json(g(name)),
  bdd_crud:create(g(path), Hammer, g(atom));

step(_Global, {step_teardown, _N, _}) -> 
  % remove hammer entry
  bdd_crud:delete(g(atom)).
