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
% 
-module(bdd_utils).
-export([assert/1, assert/2, assert_atoms/1, tokenize/1, tokenize/5, clean_line/1]).
-export([config/1, config/2, config/3, config_set/2, config_set/3, config_unset/1, config_unset/2]).
-export([alias/1, alias/2]).
-export([scenario_store/3, scenario_retrieve/3]).
-export([puts/0, puts/1, puts/2, trace/5, untrace/2]).
-export([log/5, log/4, log/3, log/2, log/1, log_level/1, depricate/4, depricate/6]).
-export([features/1, features/2, feature_name/2, os_type/0]).
-export([is_site_up/0, is_a/2, is_a/3, marker/1, parse_object/1]).
-export([json_merge/2]).
-define(NORMAL_TOKEN, 1).
-define(ESCAPED_TOKEN, 2).
-define(SUBSTITUTE_TOKEN, 3).
-define(LOG_LEVELS, [true, puts, dump, trace, debug, info, warn, error]).
-define(LOG_DEFAULT, [true, puts, info, warn, error]).
-define(LOG_TITLES, [pass, fail, skip, header, result, feature, step, step_pass, step_fail]).
-include("bdd.hrl").

assert(Bools) ->
	assert(Bools, true).
assert(Bools, debug) ->
  case assert(Bools, true) of
    true -> true;
    X -> puts("Testing result ~p", [Bools]), X
  end;
assert(Bools, Test) ->
	F = fun(X) -> case X of Test -> true; _ -> false end end,
	lists:all(F, Bools).
assert_atoms(Atoms) ->
  assert([B || {B, _} <- Atoms] ).
check(Bools) ->
  F = fun(X) -> case X of true -> true; _ -> false end end,
  lists:any(F, Bools).

% for quick debug that you want to remove later (like ruby puts)
puts()              -> log(puts, "*** HERE! ***").  
puts(Format)        -> log(puts, Format).  
puts(Format, Data)  -> log(puts, Format, Data).

% FOR PERFORMANCE, always call with Config if available!
log(Format)                       -> log(info, Format, []).
log(Format, Data)                 -> log(info, Format, Data).
log(_Config, puts, Format)         -> log(puts, Format, []);
log(_Config, Level, Format) when is_atom(Level) -> log(Level, Format, []);
log(Level, Format, Data)          -> log([], Level, Format, Data).
log(_Config, Level, Format, Data)  ->
  Logs = config(log, ?LOG_DEFAULT),
  Titles = config(titles, ?LOG_TITLES),
  Levels = Logs ++ Titles,
  Show = lists:member(Level, Levels), 
  case {Show, Level} of
    % Log methods for test results
    {true, header}    -> io:format("~nBDD TEST: " ++ Format, Data);
    {true, feature}   -> io:format("~n~nFEATURE: " ++ Format, Data);
    {true, result}    -> io:format("~n  RESULT: " ++ Format, Data);
    {true, pass}      -> io:format("~n  Passed: " ++ Format, Data);
    {true, fail}      -> io:format("~n  FAILED: " ++ Format, Data);
    {true, skip}      -> io:format("~n  ..skip: " ++ Format, Data);
    {true, step}      -> io:format("~n    Step: " ++ Format, Data);
    {true, step_pass} -> io:format("~n    Step Pass: " ++ Format, Data);
    {true, step_fail} -> io:format("~n    Step FAIL: " ++ Format, Data);
    % General Logging Ouptut
    {true, _}     -> Prefix = "   " ++ string:to_upper(atom_to_list(Level)),
                     {Module, Method, Params} = try erlang:get_stacktrace() of 
                        [{erl_parse, yecctoken_end_location, 1} | _] -> {no, trace, 0}; 
                        [{erl_parse, yecctoken_end_location, 1, _} | _] -> {no, trace, 0}; 
                        [{Mo, Me, Pa, _} | _] -> {Mo, Me, Pa}; 
                        [ST | _] -> ST; 
                        [] -> {unknown, 0, 0} 
                      catch _ -> {module, unknown, 0} end,
                      Arity = case Params of [] -> 0; X when is_number(X) -> X; X -> length(X) end,
                      case Arity of
                        0 ->  io:format("~n" ++ Prefix ++ ": " ++ Format, Data);
                        A when is_number(A) -> 
                              io:format("~n" ++ Prefix ++ ": " ++ Format ++ " <~p:~p/~p>", Data ++ [Module, Method, Arity]);
                        A ->  io:format("~n" ++ Prefix ++ ": " ++ Format ++ " <unexpected ~p>", Data++[A])
                      end;
    _ -> no_log
  end.
% helper to prefix module:method
log(Level, Module, Method, Format, Data) -> 
  PrefixFormat = atom_to_list(Module) ++ ":" ++ atom_to_list(Method) ++ " " ++ Format,
  log(Level, PrefixFormat, Data).
  
log_level(dump)       -> put(log, [dump, trace, debug, info, warn, error, puts]);
log_level(trace)      -> put(log, [trace, debug, info, warn, error, puts]);
log_level(debug)      -> put(log, [debug, info, warn, error, puts]);
log_level(info)       -> put(log, [info, warn, error, puts]);
log_level(depricate)  -> put(log, [depricate, info, warn, error, puts]);
log_level(warn)       -> put(log, [warn, error, puts]);
log_level(all)        -> put(log, all).

% helps to move code around
depricate(From, To, Method, Params) -> depricate({2014, 03, 13}, From, Method, To, Method, Params).
  
% FailDate in {YYYY, MM, DD}
depricate(FailDate, From, FMethod, To, TMethod, Params) ->
  {Year, Month, Day} = FailDate,
  FailDateSafe = if Year < 2000 -> {(Year+2000), Month, Day}; true -> FailDate end,
  {TTL, _} = calendar:time_difference({date(), time()}, {FailDateSafe, {0,0,0}}),
  {Level, Prefix} = if 
      TTL > 90 -> {debug,"Deprication:"};
      TTL > 30 -> {info,"FIX THIS >>"};
      TTL > 0  -> {warn,"GOING AWAY!"};
      true     -> {error,"DEPRICATED!"}
    end,
  log(Level,"~p `~p:~p` moved to `~p:~p` with arity ~p.",[Prefix, From, FMethod, To, TMethod, length(Params)]),
  apply(To, TMethod, Params).
  
% return the list of feature to test
features(Config) ->
  filelib:wildcard(features(Config, "*")).

% return the path to a feature to test
features(_Config, Feature) when is_atom(Feature) -> features(_Config, atom_to_list(Feature));
features(_Config, Feature) ->
  config(feature_path,"features/") ++ Feature ++ "." ++ config(extension,"feature").
  
% helper that finds the feature from the FileName
feature_name(_Config, FileName) ->
  RegEx = bdd_utils:config(feature_path,"features/") ++ "(.*)." ++ bdd_utils:config(extension,"feature"),
	{ok, RE} = re:compile(RegEx, [caseless]),
	case re:run(FileName, RE) of 
	  {match,[{0,_},{Start,Length}]} -> string:sub_string(FileName, Start+1, Start+Length);
	  _ -> FileName
	end.

% Return the file name for the test.  
trace_setup(Name, nil) ->
  trace_setup(Name, 0);

trace_setup(Name, N) ->
  SafeName = clean_line(Name),
  Prefix = config(trace_location,"/tmp/bdd_trace_"),
  string:join([Prefix, config(feature,"unknown"), "-", string:join(string:tokens(SafeName, " "), "_"), "-", integer_to_list(N), ".txt"], "").
  
trace(Name, N, Steps, Given, When) ->
  File = trace_setup(Name, N),
  {ok, S} = file:open(File, write),
  lists:foreach(fun(X) -> io:format(S, "~n==== Step ====~n~p", [X]) end, Steps),
  lists:foreach(fun(X) -> io:format(S, "~n==== Given ====~n~p", [X]) end, Given),
  [io:format(S, "~n==== When ====~n~p",[X]) || X <- (When), X =/= []],
  io:format(S, "~n==== End of Test Dump (~p) ====", [N]),
  file:close(S).

untrace(Name, N) ->
  File = trace_setup(Name, N),
  file:delete(File).

marker(Notice) ->
  URL = config(marker_url, undefined),
  Logs = config(log, []),
  Show = lists:member(debug, Logs), 
  SafePre = string:tokens(Notice," "),
  Safe = string:join(SafePre,"_"),
  case {Show, URL} of
    {_, undefined}-> false;
    {true, _}     -> eurl:get_http(eurl:path([URL, Safe]));
    _             -> false
  end.
  
% test for types
is_a(JSON, length, Count) ->
    if Count =/= length(JSON) -> log(debug, "bdd_utils:is_a(JSON,length,~p) length was ~p not ~p.",[Count, length(JSON),Count]), 
                                 log(trace, "bdd_utils:is_a JSON ~p.",[JSON]), 
                                 false;
       true                   -> true end;
is_a(JSON, Type, Key) ->
  Value = json:keyfind(JSON, Key), 
  case is_a(Type, Value) of
    true  -> true;
    X     -> 
      log(warn, "Key '~p' did not pass bdd_utils:is_a(~p, ~p). Result was ~p.", [Key, Type, Value, X]),
      false
  end.
  
is_a(Type, Value) ->
  case {Value, Type} of 
    {not_found, _} -> 
                    log(debug, "bdd_utils:is_a(~p,Value) halted because input value was `not_found`",[Type]),
                    false;    % this catches the case where there's no value there
    {_, number}  -> nomatch =/= re:run(Value, "^[\-0-9\.]*$");    % really STRING TEST
    {_, num}     -> is_number(Value);
    {_, integer} -> nomatch =/= re:run(Value, "^[\-0-9]*$");     % really STRING TEST
    {_, int} when is_list(Value) -> is_a(Type, list_to_integer(Value));
    {_, int}     -> is_integer(Value);
    {_, whole}   -> nomatch =/= re:run(Value, "^[0-9]*$");
    {_, dbid}    -> lists:member(true, [nomatch =/= re:run(Value, "^[0-9]*$"), "null" =:= Value]);
    {_, name}    -> nomatch =/= re:run(Value, "^[A-Za-z][\-_A-Za-z0-9.]*$");
    {_, boolean} -> lists:member(Value,[true,false,"true","false"]);
    {_, boolnull} -> lists:member(Value,[true,false,"true","false","null"]);
    {_, str}     -> case Value of V when is_list(V) -> check([is_list(V), length(V)=:=0]); _ -> false end; 
    {_, string}  -> is_list(Value);                              % cannot be empty
    {_, cidr}    -> nomatch =/= re:run(Value, "^([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\.([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\.([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\.([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])(\/([0-9]|1[0-9]|2[0-9]|3[0-2]))?$");
    {_, ip}      -> nomatch =/= re:run(Value, "^([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\.([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\.([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\.([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])$");
    {_, empty}   -> "" =:= Value;
    {_, hash}    -> is_tuple(Value);
    {_, list}    -> is_list(Value);
    {_, array}    -> is_list(Value);
    {_, RE} when is_list(RE) -> 
      log(trace, "bdd_utils:is_a(~p,~p) falling back to RE match", [Type, Value]),
      % fall through lets you pass in a regex (pretty cool!)
      try re:run(Value, RE) of
        nomatch        -> log(debug, "bdd_utils:is_a(~p,~p) regex not matching", [Type, Value]), false;
        {match, Match} -> log(debug, "bdd_utils:is_a(~p,~p) regex match. Output: ~p", [Type, Value, Match]), true
      catch
        X: Y -> log(error, "bdd_utils:is_a(~p,~p) RegEx Failed with ~p:~p",[Type, Value, X,Y]), false
      end;
    _        -> log(warn, "bdd_utils:is_a(~p,~p) could did not match a known type",[Type, Value]), false
  end.
	
% Web Site Cake Not Found - GLaDOS cannot test
is_site_up() ->
  URL = bdd_utils:config(url),
  log(header, "Site ~p", [URL]),
  simple_auth:authenticate_session(URL),
  case config(auth_error, undefined) of
    undefined -> true; % success
    Reason -> 
      log(error, "bdd_utils: Web site '~p' is not responding! Remediation: Check server.  Message: ~p", [URL, Reason]),
      false
  end.

% rest response specific for generic JSON parsing from http record
parse_object(Results) -> #item{data=Results#http.data , url=Results#http.url}.

% config using BIFs
config(Key) -> config(Key, undefined).
config(Key, {ListKey, Default}) ->
  List = config(Key,[]),
  case lists:keyfind(ListKey, 1, List) of
    {ListKey, Value}  -> Value;
    false             -> Default
  end;
config(Key, Default) when is_atom(Key) -> 
  case get(Key) of
    undefined -> put(Key, Default), Default;
    V         -> V
  end;

% DEPRICATING returns value for key from Config (error if not found)
config(_, Key)     -> config(Key, undefined).
config(_, Key, Default) -> config(Key, Default).

config_set(Key, {ListKey, Value}) ->
  List = config(Key, []),
  NewList = lists:keystore(ListKey, 1, List, {ListKey, Value}),
  put(Key, NewList),
  NewList;
config_set(Key, Value) ->
  put(Key, Value),
  {Key, Value}.
  
config_set(_, Key, Value)      -> config_set(Key, Value).
  
config_unset(Key)     -> put(Key, undefined).
config_unset(_, Key)  -> config_unset(Key).

% stores values used inside a scenario
scenario_store(ID, Key, Value) ->
  Scenario = get({scenario, ID}),
  Store = case Scenario of
    undefined -> [{Key, Value}];
    List      ->  % remove existing if any
                  L = case lists:keyfind(Key, 1, List) of
                    false -> List;
                    _     -> lists:keydelete(Key, 1, List)
                  end,
                  L ++ [{Key, Value}]
  end,
  put({scenario, ID}, Store),
  log(trace, "bdd_utils:scenario_store for ~p storing ~p as ~p", [ID, Key, Value]),
  Store.

% retieves values used inside a scenario
scenario_retrieve(ID, Key, Default) ->
  Scenario = get({scenario, ID}),
  Return = case Scenario of 
    undefined -> Default;
    List      -> case lists:keyfind(Key, 1, List) of
                    false     -> Default;
                    {Key, R}  -> R
                 end
  end,
  log(trace, "bdd_utils:scenario_retrieve for ~p retrieving ~p as ~p", [ID, Key, Return]),
  Return.
  
% used to handle objects & features with names that conflict w/ internal names like user or group  
alias(Class)        -> scenario_retrieve(alias_map, Class, Class).
alias(Class, MapTo) -> scenario_store(alias_map, Class, MapTo).

% removes whitespace 
clean_line(Raw) ->
	CleanLine0 = string:strip(Raw),
	CleanLine01 = string:strip(CleanLine0, left, $#),
	CleanLine1 = string:strip(CleanLine01, left, $\t),
	CleanLine11 = string:strip(CleanLine1, right, $\r),
	CleanLine2 = string:strip(CleanLine11),
	string:strip(CleanLine2, right, $.).

% converts quoted text into a list
tokenize(Step) -> tokenize(Step, false, ?NORMAL_TOKEN, [], "").

tokenize([], _IgnoreNext, TokenType, TokenList, Token ) ->
  FinalTokenList = if
    Token /= [] ->
      FinalToken = if
        TokenType == ?SUBSTITUTE_TOKEN -> token_substitute(string:strip(Token));
        true -> string:strip(Token)
      end,
      [FinalToken|TokenList];
    true -> TokenList
  end,
  lists:reverse(FinalTokenList);

tokenize(Step, IgnoreNext, TokenType, TokenList, Token ) ->
  Char = string:substr(Step,1,1),
  if
    % If this character is escaped, then just add it, even if it is a double quote
    IgnoreNext -> tokenize(string:substr(Step,2), false, TokenType, TokenList, Token ++ Char);
    % The next character is escaped, so don't add the escape
    Char == "\\" -> tokenize(string:substr(Step,2), true, TokenType, TokenList, Token);
    % Handle the first character being a double quote
    Char == "\"", Token == "" -> tokenize(string:substr(Step,2), false, ?ESCAPED_TOKEN, TokenList, "");
    % Start or end of quotes terminates the last token, whatever it was
    Char == "\"", Token /= "" ->
      NewTokenType = case TokenType of
        ?NORMAL_TOKEN -> ?ESCAPED_TOKEN;
        ?ESCAPED_TOKEN -> ?NORMAL_TOKEN;
        ?SUBSTITUTE_TOKEN -> ?SUBSTITUTE_TOKEN;
        _ -> ?NORMAL_TOKEN
      end,
      tokenize(string:substr(Step,2), false, NewTokenType, add_token(Token, TokenList), "");
    Char == "{", TokenType /= ?ESCAPED_TOKEN, Token == "" ->
      tokenize(string:substr(Step,2), false, ?SUBSTITUTE_TOKEN, TokenList, "");
    Char == "{", TokenType /= ?ESCAPED_TOKEN, Token /= "" ->
      tokenize(string:substr(Step,2), false, ?SUBSTITUTE_TOKEN, add_token(Token, TokenList), "");
    Char == "}", TokenType == ?SUBSTITUTE_TOKEN ->
      SubToken = token_substitute(string:strip(Token)),
      tokenize(string:substr(Step,2), false, ?NORMAL_TOKEN, [SubToken|TokenList], "");
    % Default action is to add to the current token
    true -> tokenize(string:substr(Step,2), false, TokenType, TokenList, Token ++ Char)
  end.

add_token(Token, TokenList) ->
  NewToken = string:strip(Token),
  case NewToken of
    "" -> TokenList;
    _ -> [NewToken|TokenList]
  end.

os_type() ->
  case os:type() of
    {win32, _}    -> windows;
    {_, nt}       -> windows;
    {unix, linux} -> linux;
    {_, Other}    -> Other    
  end.
  
% This routine is used for special subtitutions in steps that run functions or turn strings into atoms
token_substitute([$a, $p, $p, $l, $y, $: | Apply])  -> [File, Method | Params] = string:tokens(Apply, "."),
                                                              apply(list_to_atom(File), list_to_atom(Method), Params);
token_substitute([$b, $d, $d, $: | Apply])          -> token_substitute([$a, $p, $p, $l, $y, $: | Apply]);
token_substitute([$l, $o, $o, $k, $u, $p, $: | Apply]) -> [File | Params] = string:tokens(Apply, "."),
                                                              apply(list_to_atom(File), g, [list_to_atom(P) || P <- Params]);
token_substitute([$a, $t, $o, $m, $: | Apply])      -> list_to_atom(Apply);
token_substitute([$f, $i, $e, $l, $d, $s, $: | Apply]) 
                                                            -> Pairs = string:tokens(Apply, "&"),
                                                               Params = [ string:tokens(KV,"=") || KV <- Pairs],
                                                               [ {K, V} || [K, V | _] <- Params];
token_substitute([$o, $b, $j, $e, $c, $t, $: | Apply])  
                                                           -> list_to_atom(Apply);
token_substitute([$o, $: | Apply])                -> token_substitute([$o, $b, $j, $e, $c, $t, $: | Apply]);
token_substitute([$i, $n, $t, $e, $g, $e, $r, $: | Apply])
                                                           -> {Num, []} = string:to_integer(Apply),
                                                              Num;
token_substitute([$i, $n, $t, $: | Apply])        -> token_substitute([$i, $n, $t, $e, $g, $e, $r, $: | Apply]);                                                            
token_substitute(Token)                           -> Token.

json_merge(Source, []) -> Source;
json_merge(Source, [{Key, Value} | T]) ->
  NewSource = lists:keystore(Key, 1, Source, {Key, Value}),
  json_merge(NewSource, T).
