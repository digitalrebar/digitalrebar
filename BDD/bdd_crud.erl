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
-module(bdd_crud).
-export([read/1, read/2, read_id/1, read_id/2, read_obj/1, read_obj/2, fix_obj_url/1, create/2, create/3, create/4, delete/1, delete/2, update/2]).
-include("bdd.hrl").

% This is a wrapper used by the BDD framework for object level create/update/read/delete actions

% HELPERS ============================

% given path + key returns, uses the API to the the object
read(Path, Key) -> read(eurl:path([Path,Key])).
read(Path) ->
  R = eurl:get_http(Path),
  bdd_utils:log(trace, bdd_crud, read,  "obj at ~s returned ~p", [Path, R#http.code]),
  case R#http.code of
    200 ->  [R, bdd_restrat:get_object(R)];
    _   ->  [R, #obj{id= "-1"}]
  end.

% given a path + key, uses API to get the ID of the object 
read_id(Path, Key) -> read_id(eurl:path([Path, Key])).
read_id(Atom) when is_atom(Atom) ->
  O = read_obj(Atom),
  O#obj.id;
read_id(Path) -> 
  case read(Path) of
    [_]     -> "-1";
    [_, O]  -> O#obj.id
  end.

% given a path + key, uses API to get the record of the object 
read_obj(Path, Key) -> read_obj(eurl:path([Path, Key])).
read_obj(Atom) when is_atom(Atom) ->
  bdd_utils:config(Atom);
read_obj(Path) -> 
  case read(Path) of
    [_]     -> #obj{id = "-1"};
    [_, O]  -> O
  end.

% if an objet is created, it's URL does not include it's ID.  This fixes that
fix_obj_url(Obj) ->
  URL = eurl:path(Obj#obj.url, Obj#obj.id),
  Obj#obj{url=URL}.

create(Path, JSON) -> create(Path, JSON, 3).  % loop prevention

% Creates object AND adds marker to session where marker is an atom
create(Path, JSON, Marker) when is_atom(Marker) -> 
  [R, O] = create(Path, JSON),
  bdd_utils:config_set(Marker, O),
  [R, O];

% creates object based on path and json
% if successful, returns list with http & obj records
% if fails, returns list with http record
create(Path, JSON, 0) -> 
  bdd_utils:log(error, bdd_crud, create, "422 loop on create for ~p", [Path]),
  [eurl:put_post(Path, JSON, post)];
create(Path, JSON, N) ->
  R = eurl:put_post(Path, JSON, post),
  bdd_utils:log(trace, bdd_crud, create, "Code: ~p, URL: ~p", [R#http.code, R#http.url]),
  case R#http.code of
    200 ->  [R, fix_obj_url(bdd_restrat:get_object(R))];
    422 ->  Key = json:keyfind(JSON, "name"),
            bdd_utils:log(info, bdd_crud, create, "422 error. Delete object ~p and try again.",[Key]),
            delete(Path, Key),
            create(Path, JSON, N-1);
    _   ->  [R, #obj{id = "-1"}]
  end.

% Creates object AND adds marker to session where marker is the scenario id
create(Path, JSON, Marker, Key) when is_number(Marker) -> 
  [R, O] = create(Path, JSON),
  bdd_utils:scenario_store(Marker, Key, O), 
  [R, O].


% updates object based on path and json
% returns http object (and obj object if successful)
update(URI, JSON) ->
  R = eurl:put_post(URI, JSON, put),
  bdd_utils:log(trace, bdd_crud, update, "Code: ~p, URL: ~p", [R#http.code, R#http.url]),
  case R#http.code of
    200 ->  [R, bdd_restrat:get_object(R)];
    _   ->  [R]
  end.

delete(Scenario, Key) when is_number(Scenario) -> delete(bdd_utils:scenario_retrieve(Scenario, Key, undefined));
delete(Path, Key)                   -> delete(eurl:path([Path, Key])).
delete(O) when is_record(O, obj)    -> delete(O#obj.url);
delete(unknown) ->
  % handle case where no URI is passed
  bdd_utils:log(debug, bdd_crud, destroy, "Destroy was requested but Object URI was unknown (likely empty object)", []);
delete(Marker) when is_atom(Marker) -> delete(bdd_utils:config(Marker));
delete(URI) ->
  [R | _O] = read(URI),
  bdd_utils:log(trace, bdd_crud, destroy, "Code: ~p, URL: ~p", [R#http.code, R#http.url]),
  case R#http.code of
    404   -> bdd_utils:log(trace,bdd_crud, destroy,  "Removal of obj at ~s skipped: not found.", [URI]),
             [R];
    200   -> DR = eurl:delete(URI),
             bdd_utils:log(debug, bdd_crud, destroy, "Removed key ~s with ~p", [URI, DR#http.code]),
             [DR];
    _     -> bdd_utils:log(warn, bdd_crud, destroy, "unexpected result at ~s was ~p", [URI, R#http.code]),
             [R]
  end.  
