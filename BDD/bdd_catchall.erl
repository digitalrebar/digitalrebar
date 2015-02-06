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
-module(bdd_catchall).
-export([step/2]).

step(_Global, {step_given, {Scenario, _N}, ["I mark the logs with",Mark]}) -> 
  URL = bdd_utils:config(marker_url, undefined),
  S = integer_to_list(Scenario),
  SafePre = string:tokens(Mark," "),
  Safe = string:join(SafePre,"_"),
  case URL of
    undefined -> bdd_utils:log(info, bdd_catchall, stop, "could not mark because marker_url was not set",[]);
    _         -> eurl:get_http(eurl:path(URL, S)),
                 eurl:get_http(eurl:path(URL, Safe)),
                 bdd_utils:log(debug, bdd_catchall, step, "Log marked with ~p and ~p",[S, Mark])
  end;

step(_Result, {_, _N, ["pause", Time, "seconds to", Message]}) -> 
  {T, _} = string:to_integer(Time),
  io:format("\t\t\t...paused ~p seconds in order to ~s.~n", [T, Message]),
  timer:sleep(T*1000);
step(_Result, {_S, _N, ["after 1 second"]}) -> step(_Result, {_S, _N, ["after", "1", "seconds"]});
step(_Result, {_, _N, ["after", Time, "seconds"]}) -> 
  {T, _} = string:to_integer(Time),
  io:format("\t\t\tzzz...sleeping ~p seconds.~n", [T]),
  timer:sleep(T*1000);
step(_Result, {_S, _N, ["after 1 minute"]}) -> step(_Result, {_S, _N, ["after", "1", "minutes"]});
step(_Result, {_, _N, ["after", Time, "minutes"]}) -> 
  {T, _} = string:to_integer(Time),
  io:format("\t\t\tzzz...sleeping ~p minutes.~n", [T]),
  timer:sleep(T*60000);
step(_Result, {_, _N, ["after", Time, "milliseconds"]}) -> 
  {T, _} = string:to_integer(Time),
  io:format("\t\t\tzzz...sleeping ~p milliseconds.~n", [T]),
  timer:sleep(string:to_integer(T));

step( _Global, {step_setup, _N, _}) -> 
  bdd_utils:log(info, bdd_catchall, step, "No Feature Setup Step.", []);

step( _Global, {step_teardown, _N, _}) -> 
  bdd_utils:log(info, bdd_catchall, step, "No Feature Tear Down Step.", []);

step(Input, {StepType, {Scenario, StepNumber}, ["I debug BDD"]}) ->
  step(Input, {StepType, {Scenario, StepNumber}, ["I", puts, "BDD"]});
step(Input, {StepType, {Scenario, StepNumber}, ["I", Level, "BDD"]}) ->
  bdd_utils:log(Level, bdd_catchall, step, "INVESTIGATING ~s ~p:~p~n~p~n",[StepType, Scenario, StepNumber, Input]),
  true;

step(_Result, {step_given, _N, ["I do nothing to", Text]}) ->  Text;
step(_Result, {step_when, _N, ["I do nothing to", Text]}) ->  Text;
step(_Result, {step_then, _N, ["I always pass"]}) -> true;
step(_Result, {step_then, _N, ["I always fail"]}) -> false;

step(_Result, {step_given, {Scenario, _N}, ["I set",Key,"to",Value]}) ->
  bdd:log(trace, "bdd_catchall storing scenario info: Given I set ~p to ~p", [Key, Value]),
  bdd_utils:scenario_store(Scenario, Key, Value);

step( _Result, {step_given, _N, StepAction}) ->
	bdd_utils:log(warn, "ADD MISSING GIVEN STEP: ~n\tstep(_Global, {step_given, {_Scenario, _N}, ~p}) -> false;~n", [StepAction]),
	false;

step( _Result, {step_when, _N, StepAction}) ->
	bdd_utils:log(warn, "ADD MISSING WHEN STEP: ~n\tstep(_Given, {step_when, {_Scenario, _N}, ~p}) -> false;~n", [StepAction]),
	false;

step( _Result, {step_then, _N, StepAction}) ->
	bdd_utils:log(warn, "ADD MISSING THEN STEP: ~n\tstep(_Result, {step_then, {_Scenario, _N}, ~p}) -> false;~n", [StepAction]),
	false;
	
step( _Result, {step_finally, _N, StepAction}) ->
	bdd_utils:log(warn, "ADD MISSING FINALLY STEP: ~n\tstep(_Given, {step_finally, {_Scenario, _N}, ~p}) -> false;~n", [StepAction]),
	false;

step( _Result, {StepType, _N, StepAction}) ->
	bdd_utils:log(error, "UNKNOWN STEP TYPE: \"step(_, {~p, {_Scenario, _N}, ~p}) -> false;\"~n", [StepType, StepAction]),
	false;

step( _Result, StepTupple) ->
	bdd_utils:log(error, "INVALID STEP TUPPLE: Cannot resolve ~p~n", [StepTupple]),
	false.
