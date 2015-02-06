% Copyright 2013-4, Dell 
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
-module(crowbar).
-export([step/2, g/1, state/1, i18n/1, i18n/2, i18n/3, i18n/4, i18n/5, i18n/6, json/1, json/3, parse_object/1, worker/0]).
-export([json_build/1]).
-include("bdd.hrl").

g(Item) ->
  case Item of
    "cli"   -> g(cli);
    i18n    -> "utils/i18n";
    version -> "v2";
    test_node_path -> "api/test/nodes";
    queue_status -> "api/status/queue";
    cli     -> bdd_utils:config(cli, "cd ../bin && ./crowbar");
    bootenv -> node:g(bootenv);
    natural_key -> name;			% for most crowbar objects, this is the natural key.  override if not
    node_name -> "admin.bddtesting.com";
    node_atom -> admin_node;
    name    -> "bddtest";
    order   -> 9999;
    description -> "BDD Testing Only - should be automatically removed";
    error   -> -1;
    active  -> 0;
    todo    -> 1;
    transition -> 2;
    blocked -> 3;
    proposed -> 4;
    _ -> bdd_utils:log(warn, crowbar, g, "Could not resolve g request for ~p (fall through catch)", [Item]), false
  end.

state(Item) when is_atom(Item)  -> integer_to_list(g(Item));
state(Item)                     -> state(list_to_atom(Item)).

i18n(T1, T2, T3, T4, T5, T6) -> i18n_lookup([T1, T2, T3, T4, T5, T6]).
i18n(T1, T2, T3, T4, T5) -> i18n_lookup([T1, T2, T3, T4, T5]).
i18n(T1, T2, T3, T4) -> i18n_lookup([T1, T2, T3, T4]).
i18n(T1, T2, T3) -> i18n_lookup([T1, T2, T3]).
i18n(T1, T2) -> i18n_lookup([T1, T2]).
i18n(T) -> i18n_lookup([T]).
i18n_lookup(T) -> 
  Path = string:tokens(T, "+:/"),
  KeyList = case length(Path) of
    1 -> lists:nth(1, Path);
    _ -> Path
  end,
  Key = string:join(KeyList, "."),
  URI = eurl:path(["utils/i18n",Key]),
  bdd_utils:log(trace, crowbar, i18n, "looking up ~p", [URI]),
  R = eurl:get_http(URI),
  case R#http.code of
    200 -> R#http.data;
    _   -> bdd_utils:log(warn, crowbar, i18n, "Translation for ~p not found", [URI]), "!TRANSLATION MISSING!"
  end.

% rest response specific for crowbar API (only called when the vnd=crowbar)
parse_object(Results) ->
  [Type, Quantity | _] = Results#http.details,
  case Quantity of 
    "obj"   ->  JSON = json:parse(Results#http.data),
                ID = proplists:get_value("id", JSON),
                #obj{namespace = crowbar, data=JSON, type = Type, id = ID, url = Results#http.url};
    "array" ->  JSON = json:parse(Results#http.data),
                #array{namespace = crowbar, data=JSON, type = Type, url = Results#http.url, count = length(JSON) };
    "list"  ->  JSON = json:parse(Results#http.data),
                IDs = [proplists:get_value("id", I) || I <- JSON],
                #list{namespace = crowbar, data=JSON, type = Type, ids = IDs, url = Results#http.url, count = length(IDs) };
    "empty" ->  #obj{namespace = crowbar, data=none, type = Type, id = -1, url = Results#http.url };
    "error" ->  #obj{namespace = crowbar, data=error, type = Type, id = -1, url = Results#http.url };
    _       ->  bdd_utils:log(warn, "Crowbar API returned unexpected quantity flag (expected obj or list).  Returned ~p", [Results]),
                #item{namespace = crowbar, data=Results#http.data , url=Results#http.url}
  end.

json(Name, Description, Order)          -> json([{name, Name}, {description, Description}, {order, Order}]).

json(List) -> json:output(json_build(List)).

json_build([])                               -> [];
json_build({Key, Value}) when is_atom(Key)   -> json_build({atom_to_list(Key), Value});
json_build({Key, Value})                     -> {Key, Value};
json_build([Head | Tail])                    -> [ Head | json_build(Tail)].

% wait for URL to return item, loop until we get the desire result
wait_for(R, Match) when is_number(Match) ->
  case R#http.code of
    Match ->  true;
    X     ->  bdd_utils:log(debug, crowbar, wait_for, "Waiting on ~p to return ~p was ~p",[R#http.url, Match, X]), 
              false
  end;
wait_for(R, Match) ->
  case R#http.data of
    Match ->  true;
    X     ->  bdd_utils:log(debug, crowbar, wait_for, "Waiting on ~p.  Result is ~p long",[R#http.url, length(X)]), 
              false
  end.

wait_for(URL, MatchCode, MatchData, 0, _) -> 
  bdd_utils:log(error, crowbar,wait_for, "Did not get ~p/~p from ~p after repeats", [MatchCode, MatchData, URL]),
  throw("Did not get result from URL after requested # of attempts");
wait_for(URL, MatchCode, MatchData, Times, Sleep) ->
  R = eurl:get_http(URL),
  case {wait_for(R, MatchCode), wait_for(R, MatchData)} of
     {_, true} -> true;
     {true, _} -> true;
     _         -> timer:sleep(Sleep), 
                  wait_for(URL, MatchCode, MatchData, Times-1, Sleep)
  end.

worker() ->
  URL = eurl:uri(g(queue_status)),
  R = eurl:get_http(URL),
  case R#http.code of
    200 -> J = json:parse(R#http.data), list_to_integer(json:value(J,"workers")) > 0;
    _ -> false
  end.

% global setup
step(_Global, {step_setup, {_Scenario, _N}, Test}) -> 
  % setup the groups object override
  bdd_utils:log(debug, crowbar, step, "Global Setup alias: ~p",[get({scenario,alias_map})]),
  bdd_utils:alias(group, group_cb),
  bdd_utils:alias(user, user_cb),
  bdd_utils:alias(networkrange, range),
  % skip some activity if we're logging at debug level
  case lists:member(debug,get(log)) of
    true -> bdd_utils:log(debug, crowbar, global_setup, "Skipping Setup Queue Empty, Make Admin Net & Test Attribs",[]);
    _ ->
      % make sure that the delayed job queues are running
      true = bdd_clirat:step([], {foo, {0,0}, ["process", "delayed","returns", "delayed_job.([0..9])"]}),
      % turn off the delays in the test jig
      %role:step(Global, {step_given, {Scenario, _N}, ["I set the",role, "test-admin", "property", "test", "to", "false"]}), 
      %role:step(Global, {step_given, {Scenario, _N}, ["I set the",role, "test-server", "property", "test", "to", "false"]}), 
      %role:step(Global, {step_given, {Scenario, _N}, ["I set the",role, "test-client", "property", "test", "to", "false"]}), 
      %role:step(Global, {step_given, {Scenario, _N}, ["I set the",role, "test-library", "property", "test", "to", "false"]}), 
      %role:step(Global, {step_given, {Scenario, _N}, ["I set the",role, "test-discovery", "property", "test", "to", "false"]}), 
      % create admin network
      network:make_admin()
  end,
  % create node for testing
  bdd_utils:log(debug, crowbar, global_setup, "Global Setup running (creating node ~p)",[g(node_name)]),
  node:add_node(g(node_name), "crowbar-admin-node", [{description, Test ++ g(description)}, {order, 100}, {admin, "true"}], g(node_atom)),
  % setup phantom node roles
  bdd_utils:log(debug, crowbar, global_setup, "Adding Service Roles", []),
  Phantom = bdd_utils:config(system_phantom,"system-phantom.internal.local"),
  PhantomRoles = bdd_utils:config(system_phantom_roles, ["dns-service", "ntp-service","dns-mgmt_service"]),
  ServiceNRs = eurl:path([node:g(path), Phantom, "node_roles"]),
  R = eurl:get_http(ServiceNRs),
  O = bdd_restrat:get_object(R),
  bdd_utils:log(debug, crowbar, global_setup, "Checking for Phantom Node Roles ~p",[O#list.count]),
  case O#list.count of
    1 -> Attribs = ["chef-server_port", "chef-server_protocol"],
          JSON = [crowbar:json([{name, A}, {description, g(description)}, {barclamp, 'test'}, {order, g(order)}, {writable, true}]) || A <- Attribs],
          [bdd_restrat:create(attrib:g(path), J, attrib, 0) || J <- JSON],
          [node:bind(Phantom,PR) || PR <- PhantomRoles],
          node:commit(Phantom),
          node:alive(Phantom);
    _  -> noop 
  end,
  true;

% find the node from setup and remove it
step(_Global, {step_teardown, {_Scenario, _N}, _}) -> 
  worker(),
  bdd_utils:log(debug, crowbar, step, "Global Teardown running",[]),
  % skip some activity if we're logging at debug level
  case lists:member(debug,get(log)) of
    true -> bdd_utils:log(debug, crowbar, step, "Skipping Teardown Queue Empty & Admin Remove",[]);
    _ ->
      % clear quueue
      crowbar:step([], {step_given, {0, 0}, ["there are no pending Crowbar runs for",node,g(node_name)]}),
      % remove node for testing
      bdd_crud:delete(g(node_atom))
  end,
  true;

% ============================  GIVEN STEPS =========================================

step(_Given, {step_when, _N, ["I18N checks",Key]}) ->
  URI = eurl:path(g(i18n),Key),
  eurl:get_http(URI);

% we need to set alive value for new nodes
step(_Given, {step_given, {ScenarioID, _N}, ["there is a",node,Name,"marked alive"]}) -> 
  bdd_utils:scenario_store({ScenarioID, hint}, alive, "true"),
  step(_Given, {step_given, {ScenarioID, _N}, ["there is a",node,Name,"hinted"]});

step(_Global, {step_given, {ScenarioID, _N}, ["there is a hint",Hint,"with",Value]}) -> 
  bdd_utils:scenario_store({ScenarioID, hint}, Hint, Value),
  _Global;

step(_Global, {step_given, {ScenarioID, _N}, ["there is a",node,Name,"hinted"]}) -> 
  Hints = get({scenario, {ScenarioID, hint}}),
  Node = json([{name, Name}, {description, g(description)}, {order, 200}, {bootenv, node:g(bootenv)}, {alive, "true"} | Hints]),
  bdd_restrat:create(node:g(path), Node, node, ScenarioID);

step(_Global, {step_given, {ScenarioID, _N}, ["there is a",node,Name,"hinted",Hint,"as",Value]}) -> 
  bdd_utils:scenario_store({ScenarioID, hint}, Hint, Value),
  step(_Global, {step_given, {ScenarioID, _N}, ["there is a",node,Name,"hinted"]});

step(Global, {step_given, {ScenarioID, _N}, ["there is a",role, Name]}) -> 
  step(Global, {step_given, {ScenarioID, _N}, ["there is a",role, Name, "in", barclamp, "crowbar", "for", jig, "test"]});

step(_Global, {step_given, {ScenarioID, _N}, ["there is a",role, Name, "in", barclamp, Barclamp, "for", jig, Jig]}) -> 
  bdd_utils:log(debug, crowbar, step, "REST creates the ~p ~p", [role, Name]),
  JSON = json([{name, Name}, {description, role:g(description)}, {order, role:g(order)}, {barclamp, Barclamp}, {jig_name, Jig}]),
  Path = role:g(path),
  bdd_restrat:create(Path, JSON, role, ScenarioID);

step(_Global, {step_given, {ScenarioID, _N}, [deployment,Deployment,"includes",role,Role]}) -> 
  step(_Global, {step_when, {ScenarioID, _N}, [deployment,Deployment,"includes",role,Role]});
step(_Given, {step_when, {ScenarioID, _N}, [deployment,Deployment,"includes",role,Role]}) -> 
  bdd_utils:log(debug, crowbar, step, "REST addes role ~p to deployment ~p", [Role, Deployment]),
  JSON = json([{role, Role}, {deployment, Deployment}]),
  Path = deployment_role:g(path),
  bdd_restrat:create(Path, JSON, deployment_role, ScenarioID);

step(_Global, {step_given, {_Scenario, _N}, ["test loads the",File,"data into",node, Node]}) -> 
  URL = eurl:path(g(test_node_path),Node),
  JSON = json([{source, File}]),
  bdd_crud:update(URL, JSON);

% ============================  WHEN STEPS =========================================

step(_Given, {step_when, {_Scenario, _N}, ["I add",node, Node,"to",deployment, Deployment,"in",role,Role]}) -> 
  node_role:bind(Node, Role, Deployment);

step(_Given, {step_when, _N, ["REST gets the",network,Network,range,"list"]})  -> 
  % This relies on the pattern objects providing a g(path) value mapping to their root information
  URI = range:path(Network,""),
  bdd_utils:log(debug, crowbar, step, "REST range get ~p list for ~p path", [Network, URI]),
  R = eurl:get_http(URI),
  [R, bdd_restrat:get_object(R)];

step(_Given, {step_when, _N, ["REST gets the",network,Network,range,Key]})  ->
  % This relies on the pattern objects providing a g(path) value mapping to their root information
  URI = range:path(Network,Key),
  bdd_utils:log(debug, crowbar, step, "REST range get the object ~p for ~p path", [Network, URI]),
  bdd_restrat:step(_Given, {step_when, _N, ["REST requests the",URI,"page"]});

step(_Global, {_, {_Scenario, _N}, [node,Node,"has the",Role,"role"]}) -> 
  bdd_utils:log(debug, crowbar, step, "REST add role ~p to node ~p", [Role, Node]),
  node:bind(Node, Role);

step(_Given, {_, {_Scenario, _N}, [node,Node,"is committed"]}) -> 
  bdd_utils:log(debug, crowbar, step, "REST commits node ~p", [Node]),
  node:commit(Node),
  node:alive(Node);

step(_Given, {_, {_Scenario, _N}, ["REST retries",node,Node,node_role,Role]}) -> 
  bdd_utils:log(debug, crowbar, step, "REST retry Node ~p Role ~p", [Node, Role]),
  Path = eurl:path([node:g(path),Node,"node_roles"]),
  Obj = bdd_crud:read_obj(Path,Role),
  bdd_utils:log(trace, crowbar, step, "REST retry object ~p", [Obj]),
  R = eurl:path([node_role:g(path),Obj#obj.id,"retry"]),
  bdd_utils:log(debug, crowbar, step, "REST retry path ~p", [R]),
  bdd_crud:update(R, "");

% ============================  THEN STEPS =========================================


% helper for limiting checks to body
step(Result, {step_then, {_Scenario, _N}, ["I should see", Text, "in the body"]}) -> 
  bdd_webrat:step(Result, {step_then, _N, ["I should see", Text, "in section", "main_body"]});

% helper for limiting checks to body
step(Result, {step_then, {_Scenario, _N}, ["I should not see", Text, "in the body"]}) -> 
  bdd_webrat:step(Result, {step_then, _N, ["I should not see", Text, "in section", "main_body"]});

% check node status
step(Result, {step_then, {_Scenario, _N}, [node, Node, "should not be in state", State]}) -> 
  not step(Result, {step_then, {_Scenario, _N}, [node, Node, "should be in state", State]});

step(_Result, {step_then, {_Scenario, _N}, [node, Node, "should be in state", State]}) -> 
  URI = eurl:path(["api","status", "nodes", Node]),
  bdd_utils:log(debug, crowbar, step, "Node ~p checking for state ~p", [URI, State]),
  R = eurl:get_http(URI),
  {array, crowbar, "json", J, _, _, _} = bdd_restrat:get_object(R),
  bdd_utils:log(trace, crowbar, step, "Node ~p returned ~p", [Node, J]),  
  [{_ID,[{"name",Node},{"state",_},{"status",Status},_]}] = J,
  bdd_utils:log(debug, crowbar, step, "Node ~p returned ~p", [Node, Status]),  
  Status =:= State;

% ============================  CLEANUP =============================================

step(_, {_, {_Scenario, _N}, ["there are no pending Crowbar runs for",node,Node]}) -> 
  timer:sleep(250),   % we want a little pause to allow for settling
  URL = eurl:path(run:g(path),Node),
  wait_for(URL, 404, "[]", 20, 500);  % 20 times for .5 secs

% ============================  LAST RESORT =========================================
step(_Given, {step_when, _N, ["I have a test that is not in WebRat"]}) -> true;
                                    
step(_Result, {step_then, _N, ["I should use my special step file"]}) -> true.
