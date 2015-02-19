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
-module(user_cb).
-export([step/2, json_update/4, json/3, json/6, validate/1, inspector/0, g/1]).
-include("bdd.hrl").

% Commont Routine
% Provide Feature scoped strings to DRY the code
g(Item) ->
  case Item of
    path -> "/api/v2/users";
    atom1 -> bdd_user1;
    natural_key -> username; % unlike most crowbar objects, this uses username as the natural key
    username -> "oscar";
    name -> g(name);
    description -> g(email);
    order -> 100;
    email -> email();
    domain -> "crowbar.bdd";
    test_email -> "test@test.com";
    password -> "password";
    password_confirmation -> "password";
    remember_me -> "false";
    is_admin -> "false";
    _ -> crowbar:g(Item)
  end.


validate(JSON) when is_record(JSON, obj) ->
  J = JSON#obj.data,
  R = [ JSON#obj.type == "user",
        bdd_utils:is_a(J, length, 7),
        bdd_utils:is_a(J, string, created_at), 
        bdd_utils:is_a(J, string, updated_at), 
        bdd_utils:is_a(J, name, username),
        bdd_utils:is_a(J, string, email),
        bdd_utils:is_a(J, boolean, is_admin),
        bdd_utils:is_a(J, boolean, is_locked),
        bdd_utils:is_a(J, dbid, id)],
  bdd_utils:assert(R).

% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector() -> 
  crowbar_rest:inspector(user).  % shared inspector works here, but may not always

json(Name, Description, _) ->
  json(Name, Description, g(password), g(password_confirmation), g(remember_me), g(is_admin)).

% Common Routine
% Creates JSON used for POST/PUT requests
json(Username, Email, Password, Password_Confirmation, Remember_Me, Is_Admin) ->
  json:output([{"username",Username},{"email", Email},{"password", Password}, {"password_confirmation", Password_Confirmation},{"remember_me", Remember_Me},{"is_admin", Is_Admin}]).

json_update(Username, Email, Remember_Me, Is_Admin) ->
  json:output([{"username",Username},{"email", Email},{"remember_me", Remember_Me},{"is_admin", Is_Admin}]).

json_reset_password(Password) ->
  json:output([{"password", Password},{"password_confirmation", Password}]).

email()               -> email(g(username)).
email(User)           -> email(User, g(domain)).
email(User, Domain)   -> User ++integer_to_list(random:uniform(100000)) ++ "@" ++ Domain.

get_user(Username) ->
  GetPath = eurl:path([bdd_restrat:alias(user, g, [path]), Username]),
  Result = eurl:get_http(GetPath),
  bdd_utils:log(debug, users, step, "Updating user returned: ~p", [Result]),
  [Result, bdd_restrat:get_object(Result)].

user_action(Username, Action, Verb) ->
  Path = eurl:path([bdd_restrat:alias(user, g, [path]), Username, Action]),
  R = case Verb of
    delete -> eurl:delete(Path);
    post -> eurl:put_post(Path, [], post)
  end,
  bdd_utils:log(debug, "Removed user ~p ~p ~p returned: ~p", [Username, Action, Verb, R]),
  200 = R#http.code,
  get_user(Username).


%setup, takes care of create               
step(_Global, {step_setup, _N, _}) -> 
  User = json(g(username), g(email), g(password), g(password_confirmation), g(remember_me), g(is_admin)),
  bdd_crud:create(g(path), User, g(atom1));

%teardown, takes care of delete test.
step(_Global, {step_teardown, _N, _}) -> 
  bdd_crud:delete(g(atom1));

% THE FOLLOWING STEPS WILL NOT RESPOND....!!!

% GIVEN STEP =======================================================

step(_Global, {step_given, _N, ["there is not a user", Username]}) -> 
  bdd_utils:log(debug, "users:step there is not a user: ~p", [Username]),
  R = eurl:delete(g(path),Username,all),
  bdd_utils:log(debug, users, step, "there is not a user: ~p, returning: ~p", [Username,R]),
  R;

step(_Global, {step_given, _N, ["there is a user", Username, "with email", Email]}) -> 
  bdd_utils:log(debug, "users:step there is a user: ~p", [Username]),
  User = json(Username, Email, g(password), g(password_confirmation), g(remember_me), g(is_admin)),
  R = bdd_restrat:create(g(path),username,User),
  bdd_utils:log(debug, "users:step Created user: ~p", [User]),
  R;

step(_Global, {step_given, _N, ["there is a user", Username]}) -> 
  step(_Global, {step_given, _N, ["there is a user", Username, "with email", g(test_email)]});  

step(_Global, {step_given, {Scenario, _N}, ["there is an admin",user, Username]}) -> 
  bdd_utils:log(debug, users, step, "there is an admin user: ~p", [Username]),
  User = json(Username, g(test_email), g(password), g(password_confirmation), g(remember_me), true),
  R = bdd_restrat:create(g(path),User,user,Scenario),
  bdd_utils:log(debug, users, step, "Created admin user: ~p result ~p", [User, R]),
  R;

% WHEN STEP =======================================================

step( _Given, {step_when, _N, ["REST elevates user", Username, "to administrator"]}) -> 
  bdd_utils:log(debug, user, step, "Elevating user: ~p, to administrator", [Username]),
  user_action(Username, "admin", post);

step(_Given, {step_when, _N, ["REST removes admin privilege for user", Username]}) -> 
  bdd_utils:log(debug, "Removing admin privilege for user: ~p", [Username]),
  user_action(Username, "admin", delete);

step(_Given, {step_when, _N, ["REST modifies user", Username, "setting email to", Email]}) -> 
  bdd_utils:log(debug,  users, step, "Updating user: ~p, setting email to: ~p", [Username,Email]),
  JSON = json_update(Username, Email, g(remember_me), g(is_admin)),
  Path = eurl:path([bdd_restrat:alias(user, g, [path]), Username]),
  Result = eurl:put_post(Path, JSON, put),
  bdd_utils:log(debug, users, step, "Updating user returned: ~p", [Result]),
  [Result, bdd_restrat:get_object(Result)];

step(_Given, {step_when, _N, ["REST modifies", user, Username, "setting password and password_confirmation to", Password]}) -> 
  bdd_utils:log(debug, users, step, "Updating user: ~p, resetting password to: ~p", [Username,Password]),
  JSON = json_reset_password(Password),
  Path = eurl:path([bdd_restrat:alias(user, g, [path]), Username, "reset_password"]),
  Result = eurl:put_post(Path, JSON, put),
  bdd_utils:log(debug, users, step, "Updating user returned: ~p", [Result]),
  [Result, bdd_restrat:get_object(Result)];

step(_Given, {step_when, _N, ["REST locks",user, Username]}) -> 
  bdd_utils:log(debug, users, step, "Locking user: ~p", [Username]),
  user_action(Username, "lock", post);

step(_Given, {step_when, _N, ["REST unlocks",user, Username]}) -> 
   bdd_utils:log(debug, "Unlocking user: ~p", [Username]),
  user_action(Username, "lock", delete);

% THEN STEP  =======================================================

step(_Result, {step_then, _N, ["I verify",user,Username,"uses password",Password]}) ->
  % keep the current user
  BDDUser = bdd_utils:config(user),
  BDDPass = bdd_utils:config(password),
  URL = bdd_utils:config(url),
  bdd_utils:config_set(user, Username),
  bdd_utils:config_set(password, Password),
  R = simple_auth:authenticate_session(URL),
  bdd_utils:log(debug, user_cb, step, "Verify user ~p with ~p returned ~p", [Username, Password, R]),
  % put it back
  bdd_utils:config_set(user, BDDUser),
  bdd_utils:config_set(password, BDDPass),
  {auth_field, _} = simple_auth:authenticate_session(URL),
  case R of
    {auth_field, _} -> true;
    {auth_error, _} -> false;
    _ -> false
  end;

step(Result, {step_then, _N, ["the user field",Field, "should be", Value]}) -> 
   bdd_utils:log(debug, users, step, "Checking user field ~p has been set to ~p", [Field, Value]),
   R = eurl:get_result(Result, obj),
   {Field, RValue} = lists:keyfind(Field, 1, R#obj.data), 
   bdd_utils:log(debug, "users:step Checking ~p = ~p (~p)", [Value, RValue, Value == RValue]), 
   (RValue == Value).