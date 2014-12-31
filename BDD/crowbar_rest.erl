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
-module(crowbar_rest).
-export([step/2, g/1, validate_core/1, validate/1, inspector/1]).
-export([step/3, inspector/2]).  % depricate!
-include("bdd.hrl").

g(Item) ->
  case Item of
    _ -> crowbar:g(Item)
  end.

% validates JSON in a generic way common to all objects
validate_core(JSON) when is_record(JSON, obj) -> validate_core(JSON#obj.data);
validate_core(JSON) ->
  R = [bdd_utils:is_a(JSON, string, created_at), % placeholder for createdat
       bdd_utils:is_a(JSON, string, updated_at), % placgit eholder for updatedat
       bdd_utils:is_a(JSON, dbid, id)],
  bdd_utils:assert(R, debug). 

validate(JSON) when is_record(JSON, obj) -> validate(JSON#obj.data);
validate(JSON) ->
  R = [
       bdd_utils:is_a(JSON, name, name),
       bdd_utils:is_a(JSON, str, description),
       validate_core(JSON)],
  bdd_utils:assert(R, debug). 

% Common Routine - returns a list of items from the system, used for house keeping
inspector(_Config, F) -> bdd_utils:depricate({2013, 8, 1}, crowbar_rest, inspector, crowbar_rest, inspector, [F]).
inspector(Feature) ->
  R = eurl:get_http(crowbar_rest:alias(Feature, g, [path])),
  List = eurl:get_object(R), 
  [{Feature, proplist:get_value("id", O), proplist:get_value("name", O)} || O <- List#list.data].

% DEPRICATE!
step(_Config, B, C) -> bdd_utils:depricate({2013, 8, 1}, bdd_restrat, step, bdd_restrat, step, [B, C]).

% remove the node
step(_Given, {step_finally, _N, ["throw away node",Node]}) -> 
  eurl:delete([], node:g(path), Node);

% GROUPS
step(_Global, {step_given, _N, ["there is a",Category,"group",Group]}) -> 
  step(_Global, {step_given, _N, ["there is a",Category,group,Group]});
step(_Global, {step_given, {Scenario, _N}, ["there is a",Category,group,Group]}) -> 
  JSON = group_cb:json(Group, group_cb:g(description), 200, Category),
  bdd_crud:create(group_cb:g(path), JSON, Scenario, Group);

% remove the group
step(_Given, {step_finally, _N, ["throw away group",Group]}) -> 
  bdd_crud:delete(group_cb:g(path), Group);

% ============================  WHEN STEPS =========================================

step(_Given, {step_when, {_Scenario, _N}, ["REST gets the",barclamp,Barclamp,Resource,"list"]}) -> 
  Path = eurl:path([api,crowbar:g(version),barclamps,Barclamp,bdd_restrat:alias(Resource,g,[resource])]),
  bdd_utils:log(debug, crowbar, step, "REST get ~p list for ~p barclamp", [Resource, Barclamp]),
  eurl:get_http(Path);

% ============================  THEN STEPS =========================================

% validate object based on basic rules for Crowbar
step(Result, {step_then, _N, ["the object is properly formatted"]}) -> 
  JSON = eurl:get_result(Result, obj),
  validate(JSON);
  
% validate object based on it the validate method in it's ERL file (if any)
% expects an ATOM for the file
step(Result, {step_then, _N, ["the", Feature, "object is properly formatted"]}) -> 
  JSON = eurl:get_result(Result, obj), 
  bdd_restrat:alias(Feature, validate, [JSON]);

% validates a list of object IDs
step(Result, {step_then, _N, ["the object id list is properly formatted"]}) ->
  List = eurl:get_result(Result, list),
  NumberTester = fun(Value) -> bdd_utils:is_a(integer, Value) end,
  lists:all(NumberTester, List#list.ids);

% ============================  LAST RESORT =========================================
step(_Given, {step_when, _N, ["I have a test that is not in Crowbar_Rest"]}) -> true;
                                    
step(_Result, {step_then, _N, ["I should use my special step file"]}) -> true.
