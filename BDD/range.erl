% Copyright 2013, Dell 
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
-module(range).
-export([step/2, validate/1, g/1, json/3, path/2]).
-include("bdd.hrl").

% This method is used to define constants
g(Item) ->
  case Item of
    path -> "network_ranges";
    _ -> crowbar:g(Item)
  end.

% Common Routine
% Makes sure that the JSON conforms to expectations (only tests deltas)
validate(JSON) when is_record(JSON, obj) ->
  J = JSON#obj.data,
  R =[JSON#obj.type == "network_range",
      bdd_utils:is_a(J, length, 18),
      bdd_utils:is_a(J, string, name),
      bdd_utils:is_a(J, dbid, network_id),
      bdd_utils:is_a(J, str, first),
      bdd_utils:is_a(J, str, last),
      bdd_utils:is_a(J, "bmc|([([0-9]){1,2}g([0-9]){1,2}", conduit),
      bdd_utils:is_a(J, integer, vlan),
      bdd_utils:is_a(J, boolnull, use_vlan),
      bdd_utils:is_a(J, boolnull, use_bridge),
      bdd_utils:is_a(J, integer, team_mode),
      bdd_utils:is_a(J, boolnull, use_team),
      bdd_utils:is_a(J, boolean, update_dns),
      bdd_utils:is_a(J, str, dns_domain),
      bdd_utils:is_a(J, str, hostname_template),
      bdd_utils:is_a(J, str, dns_svc_name),
      bdd_utils:is_a(J, boolean, overlap),
      crowbar_rest:validate_core(J)],
  bdd_utils:assert(R);
validate(JSON) -> 
  bdd_utils:log(error, range, validate, "requires #obj record. Got ~p", [JSON]), 
  false.

path(Network, Range) -> eurl:path([network:g(path), Network, g(path), Range]).

json(Name, _Description, _Order) ->
 crowbar:json([{name, Name}, {first, "10.10.10.1/24"}, {last, "10.10.10.254/24"}]).

step(_Global, {step_setup, _N, _}) -> true;

step(_Global, {step_teardown, _N, _}) -> true;

step(_Result, {_Type, _N, ["END OF CONFIG"]}) ->
  false.
