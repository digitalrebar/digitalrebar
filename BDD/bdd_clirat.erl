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
-module(bdd_clirat).
-export([step/2]).
-export([run_cli/2]).
-include("bdd.hrl").

run_cli(Scenario, Params) ->
  CLI = bdd_utils:scenario_retrieve(Scenario, cli, bdd_utils:config(cli)),
  Username = " " ++ bdd_utils:config(cli_user_key,"-U") ++ " '" ++ bdd_utils:config(user, "NOT_GIVEN") ++ "' ",
  Password = " " ++ bdd_utils:config(cli_password_key,"-P") ++ " '" ++ bdd_utils:config(password, "NOT_GIVEN") ++ "' ",
  URL = " " ++ bdd_utils:config(cli_url_key,"-E") ++ " '" ++ bdd_utils:config(url, "NOT_GIVEN") ++ "' ",
  Cmd = CLI ++ " " ++ Params ++ Username ++ Password ++ URL,
  Out = os:cmd(Cmd),
  bdd_utils:log(debug, bdd_clirat, run_cli, "os:cmd(~p). Output ~p",[Cmd, Out]),
  Out.

pass_cli(Scenario, Params) ->
  CLI = bdd_utils:scenario_retrieve(Scenario, cli, bdd_utils:config(cli)),
  Cmd = CLI ++ " " ++ Params,
  Out = os:cmd(Cmd),
  bdd_utils:log(debug, bdd_clirat, pass_cli, "os:cmd(~p). Output ~p",[Cmd, Out]),
  Out.
  
step(_Global, {step_when, {_Scenario, _N}, ["CURL calls",Path]}) -> 
  CLI = "curl --digest",
  Username = " -u '" ++ bdd_utils:config(user, "NOT_GIVEN") ++ ":" ++ bdd_utils:config(password, "NOT_GIVEN") ++ "'",
  URL = " -i '" ++ bdd_utils:config(url, "NOT_GIVEN"),
  FullURL = eurl:path(URL, Path),
  Cmd = CLI ++ Username ++ FullURL ++ "'",
  Out = os:cmd(Cmd),
  bdd_utils:log(debug, bdd_clirat, step, "curl os:cmd(~p). Output ~p",[Cmd, Out]),
  {cli, string:tokens(Out,"\n")};

step(_Global, {step_given, {Scenario, _N}, ["CLI is",Path]}) -> 
  bdd_utils:log(debug, bdd_clirat, step, "CLI stored as ~p",[Path]),
  bdd_utils:scenario_store(Scenario, cli, Path);

step(_Given, {step_when, {Scenario, _N}, ["I run the",CMD,"command"]}) ->
  Out = run_cli(Scenario, CMD),
  {cli, string:tokens(Out,"\n")};
  
step(_Given, {step_when, {Scenario, _N}, ["I pass the",Parameters,"parameters"]}) ->
  Out = pass_cli(Scenario, Parameters),
  {cli, string:tokens(Out,"\n")};                                                            

step(Result, {step_then, {_Scenario, _N}, ["the CLI should not return",Line]}) -> 
  true =/= step(Result, {step_then, {_Scenario, _N}, ["the CLI should return",Line]});

step(Result, {step_then, {_Scenario, _N}, ["the CLI should return",Line]}) -> 
  case lists:keyfind(cli, 1, Result) of
    {cli, Outlist} -> bdd_utils:log(debug, bdd_clirat, step, "~p looking at ~p",[Line, Outlist]),
                  Eval = [re:run(R,Line) || R <- Outlist],
                  Hits = [true || E <- Eval, E=/=nomatch ],
                  length(Hits) > 0;
    _          -> false
  end;

step(_, {_Any, {_Scenario, _N}, ["process", PS, "returns", Test]}) ->
  Result = os:cmd("ps ax | grep " ++ PS),
  case re:run(Result, Test) of 
    {match, _}  -> true;
    _           -> bdd_utils:log(warn, bdd_clirat, step, "process ~p did not find ~p in ~p", [PS, Test, Result]), false
  end;

step(_Result, {_Type, _N, ["END OF CLIRAT"]}) ->
  false.
