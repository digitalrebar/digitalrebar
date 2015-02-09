% Copyright 2015, RackN
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
-module(consul).
-export([step/2, g/1, get_value/1, set_value/2]).
-include("bdd.hrl").

g(Item) ->
  case Item of
    server -> "http://127.0.0.1:8500";
    keypath -> "/v1/kv";
    _ -> bdd_utils:log(warn, consul, g, "Could not resolve g request for ~p (fall through catch)", [Item]), false
  end.

get_server() -> bdd_utils:config(consul, g(server)).

get_value(Key) ->
  URI = eurl:path([get_server(), g(keypath), Key]),
  R = simple_auth:request(URI),
  [J] = case R#http.code of
    200 -> json:parse(R#http.data);
    _   -> bdd_utils:log(warn, consul, get_value, "No Key ~p (result ~p)", [URI, R])
  end,
  {"Key", Key} = lists:keyfind("Key", 1, J), 
  {"Value", O} = lists:keyfind("Value", 1, J),
  base64:decode(O).

set_value(Key, Value) ->
  URI = eurl:path([get_server(), g(keypath), Key]),
  R = simple_auth:request(put, {URI, [], "application/text", Value}, [{timeout, 10000}], []),  
  R.

step(_Result, {step_then, _N, ["I should use my special step file"]}) -> true.
