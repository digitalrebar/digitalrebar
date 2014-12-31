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
-module(dashboard).
-export([step/2, g/1, inspector/1]).
-include("bdd.hrl").

% Common Routine
g(Item) ->
  case Item of
    path -> "dashboard";
    status_path -> node:g(status_path);
    name -> "dashboard1.example.com";
    atom -> dashboard1;
    _ -> node:g(Item)
  end.

% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector(_Config) -> [].  % add items to check

% TEMPORARY REMAPPING
% -include("bdd.hrl").
step(In, Out) -> step([], In, Out).

  
% ==== GIVEN   
step(_Config, Given, {step_when, _N, ["I examine the dashboard fingerprint"]}) -> 
  RegEx = "if\\(data\\['sum'\\] != ([0-9\\-]*)\\)",
	{ok, RE} = re:compile(RegEx, [caseless, multiline, dotall, {newline , anycrlf}]),
	[Input | _] = Given,  
	Result = re:run(Input, RE),
	Hash = case Result of
		{match, [_A, {Start, Length} | _Tail ]} -> string:substr(Input, Start+1, Length);
		_ -> no_fingerprint_found
	end,
	{fingerprint,Hash};
  
% ==== THEN
step(Config, Result, {step_then, _N, ["the dashboard fingerprint should match the REST fingerprint"]}) ->
  {fingerprint, Hash} = lists:keyfind(fingerprint, 1, Result),
  %When AJAX requests the "/framework/status/nodes" page
  JSON = eurl:get(Config, g(status_path)),
  R = json:parse(JSON),
  %Then key "fingerprint" should be a number
  {"sum", Test} = lists:keyfind("sum", 1, R),
  string:to_integer(Hash) =:= string:to_integer(Test);

step(Config, _Global, {step_setup, _N, _}) -> 
  % create node(s) for tests
  Node = node:json(g(name), g(description), 100),
  bdd_restrat:create(Config, node:g(path), g(atom), name, Node);
  
step(Config, _Global, {step_teardown, _N, _}) -> 
  % find the node from setup and remove it
  bdd_restrat:destroy(Config, g(path), g(atom)).

