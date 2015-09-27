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
-module(bdd_restrat).
-export([step/2]).
-export([get_object/1, get_result/2, parse_object/1, alias/1, alias/3, create/4]).
-include("bdd.hrl").

% HELPERS ============================

% Find records from the results
get_result(Results, Type) ->  bdd_utils:depricate({2014, 1, 1}, bdd_restrat, get_result, eurl, get_result, [Results, Type]).
  
% handles cases where objects use names that conflict w/ internal namespaces
alias(Object)                 -> bdd_utils:alias(Object).
alias(Object, Method, Params) -> 
  try apply(bdd_utils:alias(Object), Method, Params) of
    R -> R
  catch
    error: undef  -> bdd_utils:log(error, bdd_restrat, alias, "Missing ~p:~p([params]).  Object Abstraction fails.", [Object, Method]), false;
    Cause: Reason -> bdd_utils:log(error, bdd_restrat, alias, "Unexpected ~p error due to ~p.  Object ~p:~p Abstraction fails.", [Cause, Reason, Object, Method]), false
  end.

% wrapper looks at header to see if there is meta data to help wih override parsing
% if it's json then use the parser from bdd_utils
% if it's a vendor namespace then use naming convention to resolve
get_object(Result) when is_record(Result, http) -> 
  apply(Result#http.namespace, parse_object, [Result]).

% rest response for generic JSON parsing by inspecting the text
parse_object(Results)             ->
  case Results#http.data of 
    [${ | _] -> JSON = json:parse(Results#http.data),
                ID = case lists:keyfind("id", 1, JSON) of
                  {"id", FoundID} -> FoundID;
                  _-> -1 
                end,
                URL = case fromCreate of true -> eurl:path([Results#http.url, ID]); _ -> Results#http.url end,
                #obj{namespace = rest, data=JSON, id= ID, type = json, url = URL };
    [$[ | _] -> JSON = json:parse(Results#http.data),
                #list{namespace = rest, data=JSON, type = json, url = Results#http.url };
    D        -> bdd_utils:log(debug, "JSON API returned non-JSON result.  Returned ~p", [Results]),
                #obj{namespace = rest, data=D, url=Results#http.url, id = -1 }
  end.

% helper for create (so that other modules can use same process)
% path = URL
% JSON = json to post
% Object = object type atom
% Scenario = step Scenario ID
create(Path, JSON, Object, ScenarioID) ->
  Result = eurl:put_post(Path, JSON, post),
  bdd_utils:log(trace, bdd_restrat, step, "REST creates the step: PutPostResult: ~p", [Result]),
  O = get_object(Result),
  bdd_utils:scenario_store(ScenarioID, Object, O#obj.id),
  [Result, O].

array_matches(_, []) -> false;
array_matches(Find, [H | T]) ->
  case array_matches_item(Find, H) of 
    true  -> true;
    false -> array_matches(Find, T)
  end.
array_matches_item(Find, Item) ->
  case re:run(Item, Find) of 
    {match, _}  -> true;
    _           -> false
  end.

% used by steps to count lists 
item_count(Results, Key)  ->
  Obj = eurl:get_result(Results, obj),
  List = json:value(Obj#obj.data, Key),
  L = length(List),
  bdd_utils:log(debug, bdd_restrat, item_count, "result has ~p items", [length(List)]),
  L.

% GIVEN STEPS ======================
step(Global, {step_given, _N, ["there is not a",Object, Name]}) -> 
  step(Global, {step_finally, _N, ["REST deletes the",Object, Name]});

step(Global, {step_given, _N, ["there is a",Object, Name]}) -> 
  step(Global, {step_when, _N, ["REST creates the",Object,Name]});

step(_Global, {step_given, _N, ["I require a",Object, Key]}) -> 
  URI = eurl:path(alias(Object, g, [path]), Key),
  case eurl:get_page([], URI, all) of
    {200, "null"} -> bdd_utils:log(warn, bdd_restrat, step, "Required ~p object ~p was not found.  Test will likely fail",[Object, Key]);
    {200, _}      -> bdd_utils:log(debug, bdd_restrat, step, "Required ~p object ~p found.",[Object, Key]);
    {404, _}      -> bdd_utils:log(warn, bdd_restrat, step, "Required ~p object ~p not found (404).",[Object, Key]);
    {Num, _}      -> bdd_utils:log(error, bdd_restrat, step, "Required ~p object ~p error code ~p.",[Object, Key, Num])
  end;

% WHEN STEPS ======================
% use this with parameter Key is Value
step(_Given, {step_when, {Scenario, _N}, ["REST requests the", Page, "page with parameter", Key]}) ->
  step(_Given, {step_given, {Scenario, _N}, ["REST requests the", Page, "page with parameter", Key]});
step(_Global, {step_given, {Scenario, _N}, ["REST requests the", Page, "page with parameter", Key]}) ->
  Param = bdd_utils:scenario_retrieve(Scenario, Key, ""),
  URL = Page ++ "?" ++ Key ++ "=" ++ Param,
  bdd_utils:log(debug, rest_rat, step, "REST Getting ~p for page ~p + ~p=~p",[URL, Page, Key, Param]),
  R = eurl:get_http(URL),
  [R, get_object(R)];


step(_Given, {step_when, _N, ["REST requests the",Page,"page"]}) ->
  R = eurl:get_http(Page),
  [R, get_object(R)];

step(_Given, {step_when, _N, ["REST gets the",Object,"list"]}) when is_atom(Object) -> 
  % This relies on the pattern objects providing a g(path) value mapping to their root information
  URI = alias(Object, g, [path]),
  bdd_utils:log(debug, bdd_restrat, step, "REST get ~p list for ~p path", [Object, URI]),
  R = eurl:get_http(URI),
  [R, get_object(R)];

step(_Given, {step_when, _N, ["REST gets the",Object,Key]})  when is_atom(Object) ->
  % This relies on the pattern objects providing a g(path) value mapping to their root information
  URI = eurl:path(alias(Object, g, [path]), Key),
  bdd_utils:log(debug, bdd_restrat, step, "REST get the object ~p for ~p path", [Object, URI]),
  step(_Given, {step_when, _N, ["REST requests the",URI,"page"]});

step(_Given, {step_when, _N, ["REST cannot find the",Page,"page"]}) ->
  bdd_utils:log(debug, "REST cannot find the... START"),
  R = eurl:get_http(Page),
  200 =/= R#http.code;

step(_Global, {step_when, _N, ["REST creates a",Object,Name]}) -> 
  step(_Global, {step_when, _N, ["REST creates the",Object,Name]});
step(_Global, {step_given, _N, ["REST creates a",Object,Name]}) -> 
  step(_Global, {step_when, _N, ["REST creates the",Object,Name]});
step(_Global, {step_given, _N, ["REST creates the",Object,Name]}) -> 
  step(_Global, {step_when, _N, ["REST creates the",Object,Name]});

step(_Given, {step_when, {ScenarioID, _N}, ["REST creates the",Object,Name]}) -> 
  bdd_utils:log(debug, bdd_restrat, step, "REST creates the ~p ~p", [alias(Object), Name]),
  Path = alias(Object, g, [path]),
  Sample_Path = eurl:path([alias(Object, g, [path]), "sample"]),
  Sample = eurl:get_http(Sample_Path),
  {obj, rebar, _, Sample_JSON, _, _} = get_object(Sample),
  New_JSON = json:parse(alias(Object, json, [Name, alias(Object, g, [description]), alias(Object, g, [order])])),
  JSON = json:output(bdd_utils:json_merge(Sample_JSON, New_JSON)),
  create(Path, JSON, Object, ScenarioID);

step(_Given, {step_when, _N, ["REST updates the",Object,Name]}) when is_atom(Object) -> 
  JSON = alias(Object, json, [Name, alias(Object, g, [description]), alias(Object, g, [order])]),
  Path = eurl:path([alias(Object, g, [path]), Name]),
  step(_Given, {step_when, _N, ["REST updates an object at",Path,"with",JSON]});

step(_Given, {step_given, _N, ["REST updates an object at",Path,"with",JSON]}) ->
  step(_Given, {step_when, _N, ["REST updates an object at",Path,"with",JSON]});
step(_Given, {step_when, _N, ["REST updates an object at",Path,"with",JSON]}) ->
  bdd_utils:log(trace, "REST updates an object at ~p with ~p", [Path,JSON]),
  Result = eurl:put_post(Path, JSON, put),
  [Result, get_object(Result)];

step(_Then, {step_finally, _N, ["REST deletes the", Object, Name]}) -> 
  step(_Then, {step_when, _N, ["REST deletes the",Object, Name]});

step(_Given, {step_when, _N, ["REST deletes the",Object, Name]}) when is_atom(Object)-> 
  Path = alias(Object, g, [path]),
  R = bdd_crud:delete(Path, Name),
  bdd_utils:log(debug, bdd_restrat, step, "delete ~p ~p = ~p",[Object,Path, R]),
  R;
  
step(Given, {step_finally, _N, ["REST removes the",Object, Name]}) when is_atom(Object)-> 
  step(Given, {step_when, _N, ["REST deletes the",Object, Name]});
step(Given, {step_finally, _N, ["REST removes",Object, Name]}) when is_atom(Object)-> 
  step(Given, {step_when, _N, ["REST deletes the",Object, Name]});

step(Results, {step_then, _N, ["REST call returned success"]}) ->
  R = eurl:get_result(Results, http),
  case R#http.code of
    200 -> true;        % catches new format
    _   -> bdd_utils:log(debug,bdd_restrat, step, "REST call DID NOT return success! with ~p",[R]), 
           false
  end;

step(_Global, {step_when, {_Scenario, _N}, ["REST sets",Type, Name, "property", Property, "to", Value]}) -> 
  step(_Global, {step_given, {_Scenario, _N}, ["REST sets",Type, Name, "property", Property, "to", Value]});
step(_Global, {step_given, {_Scenario, _N}, ["REST sets",Type, Name, "property", Property, "to", Value]}) -> 
  Path = eurl:path([alias(Type, g, [path]), Name]),
  J = json:output([{Property,Value}]),
  bdd_utils:log(debug, bdd_restrat, step, "~p PUT ~p", [Path, J]),
  % now update 
  Result = eurl:put_post(Path, J, put),
  O = bdd_restrat:get_object(Result),
  [Result, O];

step(_Results, {step_then, _N, ["there is a", Object, Key]}) ->
  URI = alias(Object, g, [path]),
  R =eurl:get_http(eurl:path(URI, Key)),
  200 =:= R#http.code;

step(_Results, {step_then, _N, ["there is not a", Object, Key]}) ->
  URI = alias(Object, g, [path]),
  R =eurl:get_http(eurl:path(URI, Key)),
  404 =:= R#http.code;
  
step(Results, {step_then, _N, ["the", Object, "is properly formatted"]}) when is_atom(Object) ->
  % This relies on the pattern objects providing a g(path) value mapping to their root information 
  case eurl:get_result(Results, obj) of 
    not_found ->  Http = eurl:get_result(Results, http),
                  bdd_utils:log(warn, bdd_restrat, step, "code ~p from ~p", [Http#http.code, Http#http.url]), 
                  false;
    Obj       -> alias(Object, validate, [Obj])
  end;
    
step(Results, {step_then, _N, ["there should be a key",Key]}) -> 
  step(Results, {step_then, _N, ["there is a key",Key]});
step(Results, {step_then, _N, ["there is a key",Key]}) -> 
  Obj = eurl:get_result(Results, obj),
  bdd_utils:log(debug, bdd_restrat, step, "There should be a Key ~p",[Key]),
  bdd_utils:log(trace, bdd_restrat, step, "in ~p",[Obj#obj.data]),
  lists:keyfind(Key, 1, Obj#obj.data) =/= false;
                       
% you can use keys delimited by : for nesting!                                         
step(Results, {step_then, {_Scenario, _N}, ["key",Key,"is",Value]}) -> step(Results, {step_then, {_Scenario, _N}, ["key",Key,"should be",Value]});
step(Results, {step_then, {_Scenario, _N}, ["key",Key,"should be",Value]}) ->
  bdd_utils:log(debug, bdd_restrat, step, "Key ~p should be ~p",[Key, Value]),
  Obj = eurl:get_result(Results, obj),
  bdd_utils:log(trace, bdd_restrat, step, "...with data ~p",[Obj#obj.data]),
  Test = json:keyfind(Obj#obj.data, Key, ":"),
  case Value =:= Test of
     true  -> true;
     false -> bdd_utils:log(debug, bdd_restrat, step, "Key ~p expected ~p but was ~p", [Key, Value, Test]), false
  end;

step(Results, {step_then, {_Scenario, _N}, ["key",Key,"should have json",JSON,"with value",Value]}) ->
  Obj = eurl:get_result(Results, obj),
  KValue = json:keyfind(Obj#obj.data, Key, ":"),
  V = json:keyfind(KValue, JSON, ":"),
  case Value =:= V of
     true  -> true;
     false -> bdd_utils:log(debug, bdd_restrat, step, "Key ~p expected ~p:~p but was ~p", [Key, JSON, Value, V]), false
  end;

% this is really handy INSIDE a scenario because you can lookup objects that you created earlier to test IDs (assumes you only create 1 type of each)
step(Results, {step_then, {Scenario, _N}, ["key",Key,"should match id for",Type]}) -> 
  Obj = eurl:get_result(Results, obj),
  Id = bdd_utils:scenario_retrieve(Scenario, Type, "-1"),
  Test = json:keyfind(Obj#obj.data, Key, ":"),
  case Id =:= Test of
     true  -> true;
     false -> bdd_utils:log(debug, bdd_restrat, step, "Object ~p for Key ~p id expected ~p but was ~p", [Type, Key, Id, Test]), false
  end; 

step(Results, {step_then, {_Scenario, _N}, ["key",Key,"should not be",Value]}) -> 
  true =/= step(Results, {step_then, {_Scenario, _N}, ["key",Key,"should be",Value]});

step(Results, {step_then, _N, ["key",Key, "should contain",Count,"items"]}) -> 
  {C, _} = string:to_integer(Count),
  Items = item_count(Results, Key),
  Items =:= C;
                                                                
step(Results, {step_then, _N, ["key",Key,"should contain at least",Count,"items"]}) ->
  {C, _} = string:to_integer(Count),
  Items = item_count(Results, Key),
  Items >= C;

step(Results, {step_then, {_Scenario, _N}, ["key",Key,"should be a number"]}) -> 
  Obj = eurl:get_result(Results, obj),
  bdd_utils:log(debug, bdd_restrat, step, "Key ~p should be a number",[Key]),
  bdd_utils:is_a(number, json:value(Obj#obj.data, Key));

step(Results, {step_then, {_Scenario, _N}, ["key",Key,"should match", RE]}) -> 
  Obj = eurl:get_result(Results, obj),
  Match = json:value(Obj#obj.data, Key),
  case re:run(Match, RE) of
    {match, _}  -> true;
    _           -> false
  end;
                                                      
step(Results, {step_then, {_Scenario, _N}, ["key",Key, "should be an empty string"]}) -> 
  bdd_utils:is_a(empty, json:value(eurl:get_result(Results, obj), Key));
                                                      
step(R, {step_then, {S, N}, ["there should not be a value",Value]}) -> 
  step(R, {step_then, {S, N}, ["there should be a value",Value]}) =/= true;

step(Result, {step_then, {_Scenario, _N}, ["there should be a value",Value]}) -> 
  Obj = eurl:get_result(Result, obj),
  J = Obj#obj.data,
  bdd_utils:log(debug, bdd_restrat, step, "there should be a value ~p got ~p", [Value, J]),
  Test = lists:keyfind(Value,2,J),
  Test =/= false;

step(Result, {step_then, {_Scenario, _N}, ["the list should have an object with key", Key, "value", Value]}) ->
  List = eurl:get_result(Result, list),
  Values = [proplists:get_value(Key, O) || O <- List#list.data],
  Matches = [V || V <- Values, V =:= Value],
  length(Matches) > 0;
   
step(Results, {step_then, {Scenario, _N}, ["id",ID,"should have value",Value]}) -> 
  I = bdd_utils:scenario_retrieve(Scenario, ID, undefined),
  Result = eurl:get_result(Results, obj),
  R = json:value(Result, I),
  bdd_utils:log(debug, "bdd_restrat Then ID ~p (~p) with expected value ~p should be match result ~p", [ID, I, Value, R]),
  R =:= Value;

% handle JSON Arrays as returns
step(Result, {step_then, {_Scenario, _N}, ["Array contains",Item]}) -> 
  Array = eurl:get_result(Result, array),
  bdd_utils:log(debug, bdd_restrat, step, "looking for ~p in array ~p",[Item, Array#array.data]),
  lists:member(Item,Array#array.data);

step(Result, {step_then, {_Scenario, _N}, ["Array contains key",Key]}) -> 
  Array = eurl:get_result(Result, array),
  bdd_utils:log(debug, bdd_restrat, step, "looking for key ~p in array ~p",[Key, Array#array.data]),
  json:value(Array#array.data, Key) =/= ok;

step(Result, {step_then, {_Scenario, _N}, ["Array key",Key,"is an empty string"]}) -> 
  Array = eurl:get_result(Result, array),
  bdd_utils:log(debug, bdd_restrat, step, "looking for key ~p with empty string in array ~p",[Key, Array#array.data]),
  json:value(Array#array.data, Key) =:= "";

step(Result, {step_then, {_Scenario, _N}, ["Array matches",Item]}) -> 
  Array = eurl:get_result(Result, array),
  bdd_utils:log(debug, bdd_restrat, step, "looking for ~p in array ~p",[Item, Array#array.data]),
  array_matches(Item,Array#array.data);


step(Result, {step_then, {_Scenario, _N}, ["Array key", Key, "matches",Item]}) -> 
  Array = eurl:get_result(Result, array),
  {Key, Value} = lists:keyfind(Key, 1, Array#array.data),
  bdd_utils:log(debug, bdd_restrat, step, "key ~p looking for ~p in array value ~p",[Key, Item, Value]),
  array_matches_item(Item,Value);

% basic page return calls
step(Result, {step_then, _N, ["the page returns",Number]}) -> 
  step(Result, {step_then, _N, ["I get a",Number,"error"]});
step(Result, {step_then, _N, ["I get a",Number,"result"]}) -> 
  step(Result, {step_then, _N, ["I get a",Number,"error"]});
            
step(Results, {step_then, _N, ["I get a",Number,"error"]}) when is_list(Number) -> 
  Numb = list_to_integer(Number),
  step(Results, {step_then, _N, ["I get a",Numb,"error"]});
  
step(Results, {step_then, _N, ["I get a",Numb,"error"]}) -> 
  Result = eurl:get_result(Results, http),
  bdd_utils:log(debug, bdd_restrat, step, "I get a ~p error from ~p request", [Result#http.code, Result#http.url]),
  Result#http.code =:= Numb;

step(_Result, {_Type, _N, ["END OF RESTRAT"]}) ->
  false.