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
-export([step/2, g/1, get_value/1, set_value/2, get_service/1, get_node/1]).
-export([register_service/3, register_service/4]).
-include("bdd.hrl").

g(Item) ->
  case Item of
    server -> "http://127.0.0.1:8500";
    keypath -> "/v1/kv";
    servicepath -> "/v1/catalog/service";
    nodepath -> "/v1/catalog/node";
    registerpath -> "/v1/catalog/register";
    _ -> bdd_utils:log(warn, consul, g, "Could not resolve g request for ~p (fall through catch)", [Item]), false
  end.

get_server() -> bdd_utils:config(consul, g(server)).

get_value(Key) when is_atom(Key) -> get_value(atom_to_list(Key));
get_value(Key) ->
  [J] = consul_get(keypath, Key, "Key"),
  {"Value", O} = lists:keyfind("Value", 1, J),
  base64:decode(O).

set_value(Key, Value) ->
  URI = eurl:path([get_server(), g(keypath), Key]),
  R = simple_auth:request(put, {URI, [], "application/text", Value}, [{timeout, 10000}], []),  
  R.

get_service(Service) when is_atom(Service) -> get_service(atom_to_list(Service));
get_service(Service) ->
  [J] = consul_get(servicepath, Service, "ServiceName"), 
  J.

get_node(Node) when is_atom(Node) -> get_node(atom_to_list(Node));
get_node(Node) -> 
  J = consul_get(nodepath, Node, skip),
%  lists:keyfind("Node", 1, J).
  J.

register_service(Node, Address, Service) -> register_service(Node, Address, Service, []).
register_service(Node, Address, Service, _Tags) ->
  URI = eurl:path([get_server(), g(registerpath)]),
  Value = [ {"Node", Node}, {"Address", Address}, {"Service", [{ "Service", Service }]}],
  io:format("~nValue ~p",[Value]),
  JSON = json:output(Value),
  io:format("~nJSON ~s",[JSON]),
  R = simple_auth:request(put, {URI, [], "application/json", JSON}, [{timeout, 10000}], []),  
  R.

step(_Result, {step_then, _N, ["I should use my special step file"]}) -> true.

consul_get(Type, ID, IDCheck) ->
  URI = eurl:path([get_server(), g(Type), ID]),
  R = simple_auth:request(URI),
  J = case R#http.code of
    200 -> json:parse(R#http.data);
    _   -> bdd_utils:log(warn, consul, get_service, "No ~p ~p (result ~p)", [Type, URI, R])
  end,
  case IDCheck of
    skip -> noop;
    _ -> [JSON] = J, {IDCheck, ID} = lists:keyfind(IDCheck, 1, JSON)
  end,
  J.
