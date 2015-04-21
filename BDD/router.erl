% Copyright 2015, RackN
% 
% Licensed under the Apache License, Version 2.0 (the "License"); 
% you may not use this file except in compliance with the License. 
% You may obtain a copy of the License at 
% 
%  eurl://www.apache.org/licenses/LICENSE-2.0 
% 
% Unless required by applicable law or agreed to in writing, software 
% distributed under the License is distributed on an "AS IS" BASIS, 
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
% See the License for the specific language governing permissions and 
% limitations under the License. 
% 
-module(router).
-export([step/2, validate/1, g/1, json/3, path/2]).
-include("bdd.hrl").

% This method is used to define constants
g(Item) ->
  case Item of
    path -> "api/v2/" ++ g(subpath);
    subpath -> "network_routers";
    _ -> crowbar:g(Item)
  end.

% Common Routine
% Makes sure that the JSON conforms to expectations (only tests deltas)
validate(JSON) when is_record(JSON, obj) ->
  J = JSON#obj.data,
  R =[JSON#obj.type == "network_router",
      bdd_utils:is_a(J, length, 6),
      bdd_utils:is_a(J, dbid, network_id),
      bdd_utils:is_a(J, integer, pref),
      bdd_utils:is_a(J, string, address),
      crowbar_rest:validate_core(J)],
  bdd_utils:assert(R);
validate(JSON) -> 
  bdd_utils:log(error, range, validate, "requires #obj record. Got ~p", [JSON]), 
  false.

path(Network, Router) -> eurl:path([network:g(path), Network, g(path), Router]).

json(Name, _Description, Order) ->
 crowbar:json([{address, Name}, {pref, Order}, {network_id, "admin"}]).

step(_Given, {step_given, {Scenario, _N}, ["I use the Network API to create",Network,"with range",Range,"from",First,"to",Last]}) -> 
  network:step(_Given, {step_given, {Scenario, _N}, ["I use the Network API to create",Network,"with range",Range,"from",First,"to",Last]});

step(_Global, {step_given, {Scenario, _N}, ["REST creates the",network_router,Address,"on network",Network]}) -> 
  step(_Global, {step_when, {Scenario, _N}, ["REST creates the",network_router,Address,"on network",Network]});

step(_Given, {step_when, {Scenario, _N}, ["REST creates the",network_router,Address,"on network",Network]}) -> 
  JSON = crowbar:json([{address, Address}, {network, Network}]),
  bdd_utils:log(debug, router, step, "creating router ~p on network ~p with JSON ~p", [Address, Network, JSON]),
  bdd_restrat:create(g(path), JSON, router, Scenario);

step(_Given, {step_when, {_Scenario, _N}, ["REST sets",network_router,"on",Network,"item",Key,"to",Value]}) -> 
  JSON = crowbar:json([{Key, Value}]),
  Path = eurl:path([network:g(path), Network, g(subpath), "any"]),
  bdd_utils:log(debug, router, step, "updating router on network ~p with JSON ~p at ~p", [Network, JSON, Path]),
  Result = eurl:put_post(Path, JSON, put),
  [Result, bdd_restrat:get_object(Result)];

step(_Given, {step_when, {_Scenario, _N}, ["REST deletes",network_router,"on",Network]}) -> 
  Path = eurl:path([network:g(path), Network, g(subpath), "any"]),
  bdd_crud:delete(Path);

step(_Result, {step_then, {_Scenario, _N}, ["there is no",network_router,"on network",Network]}) -> 
  Path = eurl:path([network:g(path), Network, g(subpath)]),
  O = bdd_crud:read_obj(Path),
  bdd_utils:log(debug, router, step, "checking ~p", [O]),
  O#list.data =:= [[]];

step(_Global, {step_setup, {Scenario, _N}, _}) -> 
  network:step(_Global, {step_given, {Scenario, _N}, ["I use the Network API to create","testrouter","with range","general","from","10.10.99.100/24","to","10.10.99.200/24"]}),
  JSON = crowbar:json([{address, "10.10.99.1/32"}, {pref, 42}, {network, "testrouter"}]),
  bdd_restrat:create(g(path), JSON, router, Scenario),
  true;

step(_Global, {step_teardown, _N, _}) -> 
  bdd_restrat:step(_Global, {step_when, _N, ["REST deletes the",network, "testrouter"]}), 
  true;

step(_Result, {_Type, _N, ["END OF CONFIG"]}) ->
  false.
