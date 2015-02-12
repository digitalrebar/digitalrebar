% Copyright 2014, Dell 
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
-module(bdd).
-export([test/0, test/1, features/0, feature/1, feature/2, scenario/2, scenario/3, scenario/4]).
-export([debug/1, debug/2, debug/3, debug/4, failed/0, failed/1, getconfig/1, start/1, stop/1, steps/0, steps/1]).  
-export([step_run/3, step_run/4, inspect/1, is_clean/1, log/3, log/1]).
-include("bdd.hrl").

test()                   -> test("default").
test(ConfigName)         -> 
  BaseConfig = getconfig(ConfigName),
  % start the test config & run the global tests
  StartedConfig = start(BaseConfig),
  % get the list of features to test
  Features = bdd_utils:features(StartedConfig),
  %run the tests
  Complete = run([], [], Features),
  % cleanup application services
  Results = lists:filter(fun(R) -> case R of {feature, _, _, _}->true; _ -> false end end, Complete),
  File = bdd_print:file(),
  file:write_file(File,io_lib:fwrite("{test, ~p, ~p, ~p}.\n",[date(), time(),Results])),
  Final = [{Fatom, bdd_print:report(R)} || {feature, Fatom, _Feature, R} <-Results],
  Total = lists:sum([ T || {_Feature, {T, _P, _F, _, _}} <- Final]),
  Fail = lists:sum([ F || {_Feature, {_T, _P, F, _, _}} <- Final]),
  case Fail of
    0 -> log(result,"PASSED ALL TESTS (~p tests in ~p features).~n",[Total,length(Final)]),
         stop([]);
    X -> log(info,"Test Results: ~p.  Run `bdd:failed().` to re-run failed tests.",[File]),
         log(result,"FAILED ~p TESTS of ~p tests in ~p features.~n",[X, Total,length(Final)]),
         throw('FAILED >0 TESTS')
  end,
  bdd_print:html(),
  Final.
  
% list available features
features() ->
  F = bdd_utils:features([]),
  [list_to_atom(bdd_utils:feature_name([], FN)) || FN <- F].
  
% similar to test, this can be used to invoke a single feature for testing
feature(Feature) when is_atom(Feature)  -> feature("default", atom_to_list(Feature));
feature(Feature)                        -> feature("default", Feature).
feature(ConfigName, Feature) when is_atom(ConfigName), is_atom(Feature) 
                                        -> feature(atom_to_list(ConfigName), atom_to_list(Feature));
feature(ConfigName, Feature)            -> scenario(ConfigName, Feature, all).

% run one or `all` of the scenarios in a feature
scenario(Feature, ID)                  -> scenario("default", atom_to_list(Feature), ID).
scenario(ConfigName, Feature, ID) when is_atom(ConfigName), is_atom(Feature), is_number(ID)
                                       -> scenario(atom_to_list(ConfigName), atom_to_list(Feature), ID, []);
scenario(ConfigName, Feature, ID) when is_atom(ID)  
                                       -> scenario(ConfigName, Feature, ID, [puts, info, warn, error]);
scenario(ConfigName, Feature, ID) when is_number(ID)  
                                       -> scenario(ConfigName, Feature, ID, [puts, info, warn, error]);
scenario(ConfigName, Feature, Name)    -> 
  ID = erlang:phash2(Name),
  log(info, "Running Scenario ~p with feature ~p (id: ~p)", [Feature, Name, ID]),
  scenario(ConfigName, Feature, ID).
scenario(ConfigName, Feature, ID, Log) ->
  Config = bdd_utils:config_set(getconfig(ConfigName), log, Log),
  FileName = bdd_utils:features(Config, Feature),
  Result = run(Config, Feature, FileName, ID),
  stop([]),
  proplists:get_value(feature, Result).
  
% version of scenario with extra loggin turned on
debug(Feature)                    -> feature(Feature).
debug(Feature, ID)                -> debug(default, Feature, ID, debug).
debug(Config, Feature, ID) when is_number(Feature) % this handles the case where you want to set the log level with default config
                                  -> debug(default, Config, Feature, ID);
debug(Config, Feature, ID)        -> debug(Config, Feature, ID, debug).
debug(Config, Feature, ID, dump)  -> debug(Config, Feature, ID, [puts, dump, trace, debug, info, warn, error]);
debug(Config, Feature, ID, trace) -> debug(Config, Feature, ID, [puts, trace, debug, info, warn, error]);
debug(Config, Feature, ID, debug) -> debug(Config, Feature, ID, [puts, debug, info, warn, error]);
debug(Config, Feature, ID, info)  -> debug(Config, Feature, ID, [puts, info, warn, error]);
debug(Config, Feature, ID, Log) when is_number(ID)  -> 
  bdd_utils:config_set([], inspect, false),
  scenario(atom_to_list(Config), atom_to_list(Feature), ID, Log),
  bdd_utils:config_set([], inspect, true);
debug(Config, Feature, ID, Log)   -> 
  debug(Config, Feature, erlang:phash2(ID), Log).

% used after a test() run to rerun just the failed tests
failed()        -> failed(default).
failed(Config)  ->
  start(Config),
  {ok, [{test, _Date, _Time, Results} | _]} = file:consult(bdd_print:file()),
  Fails = [{Feature, lists:keyfind(fail, 1, bdd_print:result(Fails))} || {feature, Feature, _, Fails} <- Results],
  % please optimize to use just 1 global setup!
  [ failed(Config, Feature, F) || {Feature, {fail, Num, F}} <- Fails, Num > 0].
failed(_Config, Feature, [])     -> Feature;
failed(Config, Feature, [ID | T]) ->
  scenario(Config, Feature, ID),
  failed(Config, Feature, T).

% recursive runner with error catching
run(Config, [], [])                    -> Config;   % stop recursing, return config
run(Config, [], [FileName | Features]) ->
  Feature = bdd_utils:feature_name(Config, FileName),
	R = try run(Config, Feature, FileName) of
		Run -> 
	    {feature, _FAtom, _Name, Result} = lists:keyfind(feature,1,Run),
	    Out = bdd_print:result(Result),
	    {total, Total} = lists:keyfind(total, 1, Out),
	    {pass, Pass, _P} = lists:keyfind(pass, 1, Out),
	    {fail, _N, Fail} = lists:keyfind(fail, 1, Out),
	    log(result, "Feature ~p passed ~p of ~p.  Failed ~p.", [Feature, Pass, Total, Fail]),
      lists:keyfind(feature,1,Run)
	catch
		X: Y -> log(error, "bdd:run Feature ~p error ~p:~p. ~nStracktrace: ~p~n", [Feature, X, Y, erlang:get_stacktrace()]),
		        [error]
	end,
  [R | run(Config, [], Features)];

% the main runner requires you to have the feature & filename defined
run(Config, Feature, FileName)     -> run(Config, Feature, FileName, all).
run(Config, Feature, FileName, ID) ->
  % figure out the file name
  Fatom = list_to_atom(Feature),
  % start inet client if not running
  StartConfig = start(Config),                       
  % stuff the feature & file name into the config set
  FeatureConfig1 = bdd_utils:config_set(StartConfig, feature, Feature),
  FeatureConfig = bdd_utils:config_set(FeatureConfig1, file, FileName),
  % import the feature information
  {feature, Name, Scenarios} = feature_import(FileName),
  [ScenarioName, _ScenarioIn, _ScenarioWho, _ScenarioWhy | _ ] = [string:strip(S) || S <- Name, S =/= []],
  % setup UI
  log(feature, "~s (~s)", [Feature, FileName]),
  % setup the tests
  SetupConfig = step_run(FeatureConfig, [], {step_setup, {0, 0}, Feature}, [Fatom]),  % setup
  log(debug, ">>>>>>> Setup Complete, Running Tests for ~p >>>>>>>",[Feature]),
  % run the tests
  Result = {feature, Fatom, ScenarioName, [setup_scenario(SetupConfig, Scenario, ID) || Scenario <- Scenarios]},
  % tear down
  log(debug, "<<<<<<< Tests Complete, Running Tear Down for ~p <<<<<<<",[Feature]),
  step_run(SetupConfig, [], {step_teardown, {0, 9999}, Feature}, [Fatom]),  %teardown
  % return setup before we added feature stuff
  [Result | StartConfig].
	
% manual start helper for debugging
start(Config) when is_atom(Config) -> 
  C = getconfig(atom_to_list(Config)),
  start(C);
  
% critical start method used by tests
start(Config) ->
  Started = bdd_utils:config(Config, started, false),
  Global = bdd_utils:config(Config, global_setup, default),
  case Started of
    false -> 
      bdd_utils:config_unset(auth_field),    % clear field to get new token
      case application:start(inets) of
        ok          -> log(trace, "Started Inets Services",[]);
        {error,{already_started,inets}} -> log(trace, "Already Started Inets Services",[]);
        {error, A}  -> log(warn, "Errors Reported: Inets ~p",[A]);
        A           -> log(warn, "Start Reporting: Inets ~p",[A])
      end,
      case application:start(crypto) of
        ok           -> log(trace, "Started Crypto Services",[]);
        {error,{already_started,crypto}} -> log(trace, "Already Started Crypto Services",[]);
        {error, B}   -> log(warn, "Errors Reported: Crypto ~p",[B]);
        B            -> log(warn, "Start Reporting: Crypto ~p",[B])
      end,
      AzConfig = bdd_utils:is_site_up(),
      file:write_file("/tmp/inspection.list",io_lib:fwrite("~p.\n",[inspect(AzConfig)])),
      bdd_utils:marker("BDD TEST STARTING"),
      bdd_utils:log(debug, bdd, start, "running global setup using `~p:step(...step_setup...)`",[Global]),
      SetupConfig = step_run(AzConfig, [], {step_setup, {-1, 0}, "Global"}, [Global]),  
      bdd_utils:config_set(SetupConfig, started, true);
    _ -> Config
  end.

stop(Config) ->
  Started = bdd_utils:config(Config, started, false),
  Global = bdd_utils:config(Config, global_setup, default),
  case Started of
    true -> 
      TearDownConfig = step_run(Config, [], {step_teardown, {-1, 9999}, "Global"}, [Global]),
      is_clean(TearDownConfig),
      application:stop(crypto),
      application:stop(inets),
      bdd_utils:config_unset(auth_field),
      bdd_utils:config_unset(started);
    _ -> Config  
  end.

% load the configuration file
getconfig(Config) when is_atom(Config) -> getconfig(atom_to_list(Config));
getconfig(ConfigName)                  ->
  ConfigBase = case file:consult(ConfigName++".config") of
     {error,enoent} -> bdd_utils:log(error, bdd, test, "Cannot start BDD.  ~p configuration file not found.~n", [ConfigName]), throw('no config');
     {ok, CB} -> CB
  end,
  [put(K, V) || {K, V} <- ConfigBase ],
  bdd_utils:config_set(get(), config, ConfigName).
  
% read in the feature file
feature_import(FileName) ->
  Features = case file:read_file(FileName) of
    {ok, F} -> F;
    _ ->  log(error, "bdd:feature_import could not find file ~p",[FileName]), throw(bddInvalidFile)
  end,
  [Header | Body] = re:split(Features,"Scenario:"),
  Name = bdd_utils:clean_line(string:tokens(binary_to_list(Header),"\n")),
  Scenarios = [binary_to_list(S) || S <- Body],
  log(trace, "bdd:feature_import reading feature ~p with ~p scenarios",[FileName, length(Scenarios)]),
  {feature, Name, Scenarios}.
	
% run the scenarios, test list allows you to pick which tests
setup_scenario(Config, Scenario, ID) ->
  [RawName | RawSteps] = string:tokens(Scenario, "\n"),
  Name = bdd_utils:clean_line(RawName),
  [First | _ ] = Name,
  TestID = erlang:phash2(Name),
  Result = if
    First =:= $%  -> skip;
    ID =:= all    -> test_scenario(Config, RawSteps, Name);
    TestID =:= ID -> test_scenario(Config, RawSteps, Name);
    true          -> skip
  end,
  log(Result, "~s (~p)", [Name, TestID]),
  {TestID, Result}.

% pass through to bdd_utils
log(Level) -> bdd_utils:log_level(Level).
log(Level, Message, Values) -> bdd_utils:log(Level, Message, Values).

% decompose each scearion into the phrases, must be executed in the right order (Given -> When -> Then)
test_scenario(Config, RawSteps, Name) ->
  Hash = erlang:phash2(Name),
  % organize steps in the scenarios
	case scenario_steps(RawSteps, Hash) of
	  {unless, _} -> skip;
  	{N, BackwardsGivenSteps, BackwardsWhenSteps, BackwardsThenSteps, BackwardsFinalSteps}  ->
      % The steps lists are built in reverse order that they appear in the feature file in
      % accordance with erlang list building optimization.  Reverse the order here so that
      % the steps are executed in the same order as they are listed in the feature file
      GivenSteps = lists:reverse(BackwardsGivenSteps),
      WhenSteps = lists:reverse(BackwardsWhenSteps),
      ThenSteps = lists:reverse(BackwardsThenSteps),
      FinalSteps = lists:reverse(BackwardsFinalSteps),
    
    	% execute all the given steps & put their result into GIVEN
    	bdd_utils:trace(Name, N, RawSteps, ["No Given: pending next pass..."], ["No When: pending next pass..."]),
    	Given = lists:flatten([step_run(Config, [], GS) || GS <- GivenSteps]),
    	% now, excute the when steps & put the result into RESULT
    	bdd_utils:trace(Name, N, RawSteps, Given, ["No When: pending next pass..."]),
    	When = case length(WhenSteps) of
    	  0 -> Given;
    	  _ -> lists:flatten([step_run(Config, Given, WS) || WS <- WhenSteps])
    	end,
    	bdd_utils:trace(Name, N, RawSteps, Given, When),
    	% now, check the results
    	Result = lists:flatten([{step_run(Config, When, TS), TS} || TS <- ThenSteps]),
    	% safe to cleanup with the finally steps (we don't care about the result of those)
    	_Final = [{step_run(Config, Given, FS), FS} || FS <- FinalSteps],
    	% now, check the results of the then steps
    	case bdd_utils:assert_atoms(Result) of
    		true -> bdd_utils:untrace(Name, N), pass;
    		_ -> log(info, "*** FAILURE REPORT FOR ~p (~p) ***",[Name, Hash]), bdd_print:fail(lists:reverse(Result)), fail
    	end;
    skip -> skip;
    X -> 
      log(error,"bdd:test_scenario: Unknown scenario_steps result ~p", [X]), 
      fail
  end.

% Inital request to run a step does not know where to look for the code, it will iterate until it finds the step match or fails
step_run(Config, Input, Step) ->
	StepFiles = [list_to_atom(bdd_utils:config(feature)) | bdd_utils:config(secondary_step_files, [bdd_webrat, bdd_catchall])],
  step_run(Config, Input, Step, StepFiles).
	
% recursive attempts to run steps
step_run(Config, Input, Step, [Feature | Features]) ->
  % sometimes, we have to rename files, the alias lets the system handle that
  Alias = bdd_utils:alias(Feature),
  {_, {Scenario, StepNum}, _} = Step, 
  % now we need to try and run the feature
	try apply(Alias, step, [Input, Step]) of
		error -> 
		  {error, Step};
		Result -> 
		  bdd_utils:marker(io_lib:format("STEP RESULT (Scenario ~p:~p)", [Scenario, StepNum])),
		  log(debug, "^^ ~p,~p Ran ~p:step(R, ~p).",[Scenario, StepNum, Alias,Step]),
      log(trace, "^^ ~p,~p Input -> ~p",[Scenario, StepNum, Input]),
		  log(trace, "^^ ~p,~p Result -> ~p",[Scenario, StepNum, Result]),
		  Result
	catch
		error: undef -> 
      % if this is a given step then we should try it as a when before we give up
      Retry = if is_record(Step, stepgiven) ->
                RWhen = #stepwhen{scenario=Step#stepgiven.scenario, step=Step#stepgiven.step},
                step_run(Config, Input, RWhen, Feature);
              true -> error
              end,
      % if we did not pass (or have) an alternate then run in the other feature list
      case Retry of
        error -> step_run(Config, Input, Step, Features);
        _     -> Retry
      end;
		error: function_clause -> step_run(Config, Input, Step, Features);
		exit: {noproc, {gen_server, call, Details}} -> 
		  log(info,  "exit Did not find step: ~p", [Alias]),
      log(error, "web server not responding.  Details: ~p",[Details]), 
      throw("BDD ERROR: Could not connect to web server.");
    error: {badmatch, {error, no_scheme}} ->
		  log(info,  "badmatch in code due to no_scheme.",[]), 
      log(debug, "Stacktrace: ~p~n", [erlang:get_stacktrace()]),
      log(error, "Attempted \"feature ~p, step ~p.\"",[Alias, Step]),
		  error; 
		X: Y -> 
		  bdd_utils:marker(io_lib:format("BDD ERROR (Scenario ~p:~p)", [Scenario, StepNum])),
		  log(info,  "step run found ~p:~p", [X, Y]), 
      log(debug, "Stacktrace: ~p", [erlang:get_stacktrace()]),
      log(error, "Attempted \"apply(~p, step, [[Input], ~p]).\"",[Alias, Step]),
		  error 
	end;

% we don't want to FAIL for missing setup and teardown steps	
step_run(Config, _Input, {step_setup, _, Feature}, [])    
                  -> log(debug, "Feature ~p has no Setup defined (or it throws an error).", [Feature]), Config;
step_run(Config, _Input, {step_teardown, _, Feature}, []) 
                  -> log(debug, "Feature ~p has no Teardown defined (or it throws an error).", [Feature]), Config;
	
% no more places to try, fail and tell the user to create the missing step
step_run(_Config, _Input, Step, []) ->
	bdd_utils:log(error, bdd, step_run, "Unable to resolve step ~p!", [Step]),
	throw("FAIL: no matching expression found for Step"), 
	error.
	
% Split our steps into discrete types for sequential processing
% Each step is given a line number to help w/ debug
% most steps return step tuple
% escape clauses ("unless") returns the escape tuple
scenario_steps(Steps, ScenarioID) ->
	scenario_steps(Steps, 1, [], [], [], [], unknown, ScenarioID).
scenario_steps([H | T], N, Given, When, Then, Finally, LastStep, ScenarioID) ->
	CleanStep = bdd_utils:clean_line(H),
	{Type, StepRaw} = step_type(CleanStep),
	StepPrep = {Type, bdd_utils:tokenize(StepRaw)},
	Step = case StepPrep of
	  {step_and, SS} -> {LastStep, SS};
	  {Type, SS} -> {Type, SS}
	end,
	% calculate for the skips
	OStype = bdd_utils:os_type(),
	case Step of
	  {step_skip, S}    ->  log(info,"Skipping ~p ~s", [ScenarioID, S]), 
                    	    skip;
	  {step_while, S}    -> While = [binary_to_atom(A, utf8) || A <- re:split(S," ")],
                          Env = bdd_utils:config(environment, undefined),
                          % while list is not included in env list then skip  (opposite of while)
                          WhileEnv = lists:member(Env, While),
                          WhileOS = lists:member(OStype, While),
                    	    if WhileEnv; WhileOS ->
                    	        log(debug,"bdd:test_scenario: while running ~p [~p/P on ~p in ~p]", [ScenarioID, Env, OStype, While]),
                              scenario_steps(T, N, Given, When, Then, Finally, step_while, ScenarioID);
                    	      true -> log(debug,"While skipping ~p [~p/~p not in ~p]", [ScenarioID, Env, OStype, While]), 
                    	        skip
                    	    end;
		{step_unless, S}  ->  Unless = [binary_to_atom(A, utf8) || A <- re:split(S," ")],
                          Env = bdd_utils:config(environment, undefined),
                          % unless list is included in env list then skip (opposite of unless)
                          UnlessEnv = lists:member(Env, Unless),
                          UnlessOS = lists:member(OStype, Unless),
                    	    if UnlessEnv; UnlessOS ->
                	            log(debug,"Unless skipping ~p [~p/~p  in ~p]", [ScenarioID, Env, OStype, Unless]), 
                    	        skip;
                    	      true -> log(debug,"bdd:test_scenario: running unless ~p [~p/~p not in ~p]", [ScenarioID, Env, OStype, Unless]),
                              scenario_steps(T, N, Given, When, Then, Finally, step_unless, ScenarioID)
                    	    end;
		{step_given, S}   -> scenario_steps(T, N+1, [{step_given, {ScenarioID, N}, S} | Given], When, Then, Finally, step_given, ScenarioID);
		{step_when, S}    -> scenario_steps(T, N+1, Given, [{step_when, {ScenarioID, N}, S} | When], Then, Finally, step_when, ScenarioID);
		{step_then, S}    -> scenario_steps(T, N+1, Given, When, [{step_then, {ScenarioID, N}, S} | Then], Finally, step_then, ScenarioID);
		{step_finally, S} -> scenario_steps(T, N+1, Given, When, Then, [{step_finally, {ScenarioID, N}, S} | Finally], step_finally, ScenarioID);
		{empty, _}        -> scenario_steps(T, N,   Given, When, Then, Finally, empty, ScenarioID);
		{unknown, Myst}   -> log(warn, "bdd: No prefix match for ~p in Scenario #~p", [Myst, ScenarioID]), 
		                     scenario_steps(T, N+1, Given, When, Then, Finally, unknown, ScenarioID)		
	end;
scenario_steps([], N, Given, When, Then, Finally, _, _ScenarioID ) ->
	% returns number of steps and breaks list into types, may be expanded for more times in the future!
	{N, Given, When, Then, Finally}.
	
% inspect system to ensure that we have not altered it
% this relies on the features implmenting the inspect meth
inspect(Config) ->
  % only inspect if inspect flag is not false
  case bdd_utils:config(inspect, true) of
    false -> [noop];
    _ ->  Features = bdd_utils:features(Config),
          inspect(Config, [], [list_to_atom(bdd_utils:feature_name(Config,F)) || F <- Features])
  end.

inspect(_Config, Result, []) -> Result;
inspect(Config, Result, [Feature | Features]) ->
  % sometimes, we have to rename files, the alias lets the system handle that
  Alias = bdd_utils:alias(Feature),
  try apply(Alias, inspector, []) of
		R -> R ++ inspect(Config, Result, Features)
	catch
		_X: _Y -> inspect(Config, Result, Features) % do nothing, we just ignore lack of inspectors
	end.
	
is_clean(Config) -> 
  {ok, [Inspect]} = file:consult("/tmp/inspection.list"),
  is_clean(Config, Inspect).
is_clean(Config, StartState) ->
  EndState = inspect(Config),
  Diff = lists:subtract(StartState, EndState),
  case Diff of
    []      -> true;
    % TODO - cleanup should tell you if the artifacts are from BEFORE or AFTER.  Right now, it is not clear!
    Orphans -> log(warn, "BDD:is_clean Inspector Reports tests did NOT CLEANUP all artifacts!~n\tOrphans: ~p.~n",[Orphans]),
               false
  end.

% figure out what type of step we are doing (GIVEN, WHEN, THEN, etc), return value
step_type([$S, $k, $i, $p, 32 | Skip])            ->  { step_while, Skip};    % temporary redirect until I fix the logic
step_type([$W, $h, $i, $l, $e, 32 | While])       ->  { step_while, While};
step_type([$U, $n, $l, $e, $s, $s, 32 | Unless])  ->  { step_unless, Unless};
step_type([$G, $i, $v, $e, $n, 32 | Given])       ->	{ step_given, Given };
step_type([$W, $h, $e, $n, 32 | When] )           ->	{ step_when, When };
step_type([$T, $h, $e, $n, 32 | Then ])           ->  { step_then, Then };
step_type([$F, $i, $n, $a, $l, $l, $y, 32 | F ])  ->  { step_finally, F };
step_type([$A, $n, $d, 32 | Next ])               ->  { step_and, Next };
step_type([])                                     ->  { empty, []};
step_type(Step)                                   ->  { unknown, Step }.

%utilities to create step information from code
steps_output(RE, Step) ->
	{Type, S} = case re:run(Step, RE) of
	  {match, [_A, {T1, T2}, _B, {S1, S2} | _]} -> {string:substr(Step,T1+1, T2), string:substr(Step,S1+1, S2)};
	  nomatch -> {"other", Step}
	end,
	io:format("  * ~s ~s~n",[Type, S]).

steps() ->
  Files = filelib:wildcard("*.erl"),
  [steps(File) || File <- Files].
  
steps(File) ->
  io:format("* ~s~n",[File]),
  {ok, RawCode} = file:read_file(File),
	RawLines = string:tokens(binary_to_list(RawCode),"\n"),
	{ok, RE} = re:compile("\\{step_([a-z]*),(.*)\\[(.*)\\]\\}"),	
	StepLines = [S || S <- RawLines, string:substr(S,1,5) =:= [$s, $t, $e, $p, $( ]],
	[steps_output(RE, S) || S <- StepLines],
  io:format("~n").
  
