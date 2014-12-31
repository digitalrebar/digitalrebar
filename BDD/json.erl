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

-module(json).
-export([parse/1, value/2, output/1, pretty/1, keyfind/2, keyfind/3]).
-export([json_array/3, json_value/2, json_safe/3]).
-export([json_value_quoted/2]).
-record(json, {list=[], raw=[]}).
-record(jsonkv, {value=[], raw=[]}).

% used for nested keys delimited by Token, resolved recursively
keyfind(JSON, Key, Token)          -> keyfindtokens(JSON, string:tokens(Key, Token)).
keyfindtokens(JSON, [Key | Keys])  -> 
  % protect from excessive delimiting
  Data = string:join(string:tokens(JSON,"\\\""),""),
  keyfindtokens(keyfind(Data, Key), Keys);
keyfindtokens(JSON, [])            -> JSON.


keyfind(JSON, Key) when is_atom(Key) -> keyfind(JSON, atom_to_list(Key));
keyfind(JSON, Key)                   ->
  J = json_safe(JSON, keyfind, no_warn),
  case lists:keyfind(Key, 1, J) of
    {Key, R} -> R;
    false -> not_found;
    _ -> error
  end.

value_list(JSON, [Key | []]) ->  
  value_item(JSON, Key);
value_list(JSON, [Key | Tail]) ->  
  value_list(value_item(JSON, Key), Tail).
value_item(JSON, Key) ->           
  K = string:strip(Key, left, $[),
  try lists:keyfind(K, 1, JSON) of
    {K, V} -> V;
    false  -> bdd_utils:log(debug, "json:value_item did not find key ~p in ~p",[Key, JSON]);
    X      -> bdd_utils:log(warn, "json:value_item got unexpected result ~p when looking for key ~p in ~p",[X, Key, JSON])
  catch
    E      -> bdd_utils:log(error, "json:value_item threw an error ~p when looking for key ~p in ~p",[E, Key, JSON])
  end.
  
value(JSON, Key) ->    
  List = string:tokens(Key, "]"),
  value_list(JSON, List).

% handles \" escaped quotes
json_value_quoted(Value, ["\\\"" | T]) ->
  #jsonkv{value=Value, raw=T};

% handles \" escaped quotes
json_value_quoted(Value, [$\\, $" | T]) ->
  #jsonkv{value=Value, raw=T};

% handles values that are quoted (this one ends the quote)
json_value_quoted(Value, [$" | T]) ->
  #jsonkv{value=Value, raw=T};
  
json_value_quoted(Value, [Next | T]) ->
  json_value_quoted(Value ++ [Next], T).
  
% returns JSON Key Values with remaining JSON
json_value(Value, RawJSON) ->
  [Next | T] = RawJSON, 
  case Next of
% omit this test because the json should be formatted correctly, this is reall a cheat for bad quoting!
%    $: -> bdd_utils:log(error, json, json_value, "unexpected : before ~p building value ~p", [T, Value]), 
%          throw('unexpected token : in value');
    $" -> json_value_quoted(Value, T);                        % run to next quote,exit
    ${ -> J = json(#json{raw=RawJSON}, []),                   % recurse to get list
            #jsonkv{value=J#json.list, raw=J#json.raw};  
    $[ -> J = json_array(0, [], T),                    % recurse to get array using 0++ as the Key
            #jsonkv{value=J#json.list, raw=J#json.raw};  
    $, -> #jsonkv{value=string:strip(Value), raw=RawJSON};    % terminator, return
    $} -> #jsonkv{value=string:strip(Value), raw=RawJSON};    % terminator, return
    $] -> #jsonkv{value=string:strip(Value), raw=RawJSON};    % terminator, return
    _ -> json_value(Value ++ [Next], T)                       % recurse
  end.

% parses an Array of values using numbers as the keys
json_array(Index, Value, RawJSON) ->
  [Next | T] = RawJSON, 
  case {Value, Next} of
    {_, $:} -> throw('unexpected token : in array');
    {_, $}} -> throw('unexpected token } in array');
    {_, $[} -> throw('unexpected token [ in array');
    {_, ${}  -> J = json(#json{raw=RawJSON}, []),               % recurse to get sublist
                json_array(Index, J#json.list, J#json.raw);     % continue w/ value from sublist
    {[], $,} -> json_array(Index, [], T);                       % no value, but recurse to get next element
    {V, $,} ->  List = json_array(Index+1, [], T),              % more items, go get them
               #json{list=lists:merge([string:strip(V)], List#json.list), raw=List#json.raw};      % recurse to get next element
    {V, $]} -> #json{list=[string:strip(V)], raw=T};            % terminator, return
    {V, $"} -> J = json_value_quoted(V, T),                     % run to next quote,exit
                % jvq may not return a list, if not, keep going
                if is_record(J, jsonkv) ->
                      Item = J#jsonkv.value,
                      Rest = json_array(Index+1, [], J#jsonkv.raw),
                      List = if Rest#json.list =/= [[]] -> [Item | Rest#json.list];
                                true -> [Item] end,
                      #json{list=List, raw=Rest#json.raw};
                   % this is the expected return, stop
                   is_record(J, json) -> J;  
                   % ouch!
                   true -> throw('unexpected return for json_value_quoted')
                end;
    {V, _} ->  json_array(Index, V ++ [Next], T)                % recurse
  end.
  
% parses the Key Value pairs (KVPs) based on , & } delimiters
json(JSON, Key) ->
  [Next | T] = JSON#json.raw,
  case {Next, T} of
    {$\\, _} -> json(JSON#json{raw=T}, Key);        % ignore
    {$", _}  -> KV = json_value_quoted([], T),    % using the value parser to get the key
                K = KV#jsonkv.value,              % so the key is returned as the value
                V = KV#jsonkv.raw,            
                json(JSON#json{raw=V}, K);         % strip out the quoted text as the key, loop
    {$\n, _} -> json(JSON#json{raw=T}, Key);        % ignore
    {${, _}  -> json(#json{raw=T}, []);             % start new hash
    {$[, _}  -> JSON2 = json_array(0, [], T), JSON2#json.list;
    {$,, _}  -> json(JSON#json{raw=T}, []);         % add new value
    {$:, _}  -> KV = json_value([], T),  % get value for key
             List = lists:merge(JSON#json.list, [{string:strip(Key), KV#jsonkv.value}]),
             json(#json{list=List, raw=KV#jsonkv.raw}, []);  % add new KVP
    {$}, []} -> JSON#json.list;                    %DONE!
    {$}, _}  -> JSON#json{raw=T};                   %List parse, but more remains!
    {_, _}   -> json(JSON#json{raw=T}, Key ++ [Next])  % add to key
  end.

% entry point
parse(RawJSON) ->
  % make sure that this needs to be parsed!
  case RawJSON of 
    "null"            -> [];
    [${ | _]          -> json(#json{raw=RawJSON}, []);
    [$[ | _]          -> json(#json{raw=RawJSON}, []);
    [{_, _} | _] when is_list(RawJSON) 
                      -> bdd_utils:log(debug, "calling json:parse with information that is already parsed.  Taking no action, but thought you should know.",[]),
                         RawJSON;    % this in the expected format, it's ok
    _                 -> bdd_utils:log(warn,"json:parse input did not match expected format.  Input: ~p",[RawJSON])
  end.    
  
% Pretty Output of List
pretty(List) -> 
  pretty(List, "  "),
  io:format("~n").
pretty([], _Level) -> noop;
pretty([Head | List], Level) ->
  pout(Head, Level),
  pretty(List, Level).
pout({K,V}, Level) -> 
  case V of
    [{_K, _V} | _L] -> io:format("~n~s~p:",[Level, K]),
                       pretty(V, Level++"  ");
    _               -> io:format("~n~s~p: ~p",[Level, K, V])
  end.

% CREATE JSON STRING FROM List: [{K, V}, {K, V}, {K, [{K, V}, ...]}, ...]
% create json from list
output({json, List}) -> output(List);
output(List)         -> lists:concat(["{", output_inner(List), "}"]).

% break List into smaller parts
atomize({K, [{K1, V1}]}) when is_integer(K1)     -> lists:concat([quote_key(K),":[", output(V1), "]"]);
atomize({K, [{K1, V1}]})                         -> lists:concat([quote_key(K),":", output([{K1, V1}])]);
atomize({K, [{K1, V1} | T]}) when is_integer(K1) -> lists:concat([quote_key(K),":[", output(V1), ", ", array(T), "]"]);
atomize({K, [{K1, V1} | T]})                     -> lists:concat([quote_key(K),":", output([{K1, V1} | T])]);
atomize({K, V})                                  -> lists:concat([quote_key(K),":\"", V, "\""]).

% strip out the index for arrays
array([{K, V}])     when is_integer(K) -> output(V);
array([{K, V} | T]) when is_integer(K) -> lists:concat([output(V), ", ", array(T)]).

% quote strings, no not quote number keys
quote_key(K) when is_integer(K) -> K;
quote_key(K)                    -> lists:concat(["\"",K,"\""]).

% recurse the list
output_inner([Head | []]) ->
  atomize(Head);
output_inner([Head | Tail]) ->
  atomize(Head) ++ ", " ++ output_inner(Tail).


% handle case where we are given raw json by mistake
% From is the calling routine for logging
% Warn=true will turn on verbose warnings
json_safe(JSON, From, Warn) ->
  case JSON of
    [${ | _] -> parse(JSON);
    [$[ | _] -> parse(JSON);
    [{_, _} | _] when is_list(JSON) 
             -> if Warn == true -> bdd_utils:log(debug, "json:~p with information that is already parsed.  Taking no action, but thought you should know.",[From]); true -> noop end,
                JSON; % this in the expected format, it's ok
    _       ->  bdd_utils:log(warn, "json:~p with information that is not correctly formatted: ~p.",[From, JSON]), 
                JSON
  end.
