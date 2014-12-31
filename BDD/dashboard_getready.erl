% Copyright 2014, Rob Hirschfeld
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
-module(dashboard_getready).
-export([step/2, g/1, inspector/1]).
-include("bdd.hrl").

% Common Routine
g(Item) ->
  case Item of
    path -> "dashboard/getready";
    name -> "ready.set.go";
    atom -> getready1;
    _ -> dashboard:g(Item)
  end.

% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector(_Config) -> [].  % add items to check


step(_Global, {step_setup, _N, _}) -> 
  % create node(s) for tests
  Node = node:json(g(name), g(description), 100),
  [_,O] = bdd_crud:create(node:g(path), Node, g(atom)),
  bdd_utils:log(debug, dashboard_getready, step_setup, "Created GetReady Node ~p", [O#obj.id]);
  
step(_Global, {step_teardown, _N, _}) -> 
  % find the node from setup and remove it
  bdd_crud:delete(g(atom)).
