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
-module(barclamp).
-export([step/2, step/3, json/3, validate/1, inspector/1, g/1]).
-include("bdd.hrl").

% Commont Routine
% Provide Feature scoped strings to DRY the code
g(Item) ->
  case Item of
    path -> "api/v2/barclamps";
    name -> "bddbarclamp";
    atom -> barclamp1;
    _ -> rebar:g(Item)
  end.
  
% Common Routine
% Makes sure that the JSON conforms to expectations (only tests deltas)
validate(JSON) when is_record(JSON, obj) ->
  J = JSON#obj.data,
  R =[JSON#obj.type == "barclamp",
      bdd_utils:is_a(J, string, version), 
      bdd_utils:is_a(J, string, build_version), 
      bdd_utils:is_a(J, string, commit), 
      bdd_utils:is_a(J, string, build_on), 
      bdd_utils:is_a(J, string, source_path), 
      bdd_utils:is_a(J, string, source_url), 
      bdd_utils:is_a(J, dbid, barclamp_id), 
      bdd_utils:is_a(J, string, cfg_data),
      bdd_utils:is_a(J, length, 13),
      rebar_rest:validate(J)],
  bdd_utils:assert(R, debug);
validate(JSON) -> 
  bdd_utils:log(error, barclamp, validate, "requires #obj record. Got ~p", [JSON]), 
  false.

% Common Routine
% Creates JSON used for POST/PUT requests
json(Name, Description, Order) ->
  json:output([{"name",Name},{"description", Description},{"order", Order}]).

% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector(Config) -> 
  rebar_rest:inspector(Config, barclamp).  % shared inspector works here, but may not always

% DEPRICATE!
step(_Config, B, C) -> bdd_utils:depricate({2013, 8, 1}, barclamp, step, barclamp, step, [B, C]).

step(_Global, {step_setup, _N, _}) -> true;

step(_Global, {step_teardown, _N, _}) -> true.
