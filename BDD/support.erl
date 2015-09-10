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
-module(support).
-export([step/2, g/1]).
-include("bdd.hrl").

% Commont Routine
% Provide Feature scoped strings to DRY the code
g(Item) ->
  case Item of
    setting -> "utils/settings";
    _ -> rebar:g(Item)
  end.

step(_Global, {step_given, {_Scenario, _N}, ["I set",user,"setting",Key,"to",Value]}) -> 
  URI = eurl:uri(g(setting)),
  JSON = json:output([{Key, Value}]),
  eurl:put_post(URI, JSON, put);

step(_Given, {step_when, {_Scenario, _N}, ["I inspect the logs for",Mark]}) -> 
  Path = bdd_utils:config(marker_log, "../rails/log/development.log"),
  step(_Given, {step_when, {_Scenario, _N}, ["I inspect the",Path,"for",Mark]});

step(_Given, {step_when, {_Scenario, _N}, ["I inspect the",Path,"for",Mark]}) -> 
  Lines = bdd_utils:config(tail_lines, 100),
  Cmd = "tail -n " ++ integer_to_list(Lines) ++ " '" ++ Path ++ "' | grep '" ++ Mark ++ "'",
  Out = os:cmd(Cmd),
  bdd_utils:log(trace, support, step, "~p output ~p",[Cmd, Out]),
  #grep{data = string:tokens(Out,"\n")};

step(Result, {step_then, {_Scenario, _N}, ["I should grep",Grep]}) -> 
  {grep, In} = eurl:get_result(Result, grep),
  bdd_utils:log(debug, support, step, "~p looking at ~p",[Grep, In]),
  Eval = [re:run(R,Grep) || R <- In],
  Hits = [true || E <- Eval, E=/=nomatch ],
  length(Hits) > 0.
