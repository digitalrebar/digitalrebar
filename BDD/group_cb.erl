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
-module(group_cb).
-export([step/2, step/3, json/3, json/4, g/1, validate/1, inspector/0]).
-include("bdd.hrl").	
	
g(Item) ->
  case Item of
    categories -> ["ui","rack","tag"];
    path -> "/api/v2/groups";
    name1 -> "bddthings";
    atom1 -> group1;
    name2 -> "bdddelete";
    atom2 -> group2;
    name_node1 -> "group1.node.test";
    atom_node1 -> gnode1;
    _ -> crowbar:g(Item)
  end.

validate(JSON) when is_record(JSON, obj) ->
  J = JSON#obj.data,
  Category = json:keyfind(J, category),
  R = [lists:member(Category,g(categories)), 
       bdd_utils:is_a(J, length, 7),
       crowbar_rest:validate(J)],
  bdd_utils:assert(R).

% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector() -> 
  bdd_restrat:inspector(group).  % shared inspector works here, but may not always
  
% Returns the JSON List Nodes in the Group
get_group_nodes(Group) ->
  URI = eurl:path([g(path),Group, "nodes"]),
  bdd_utils:log(debug, group_cb, get_group_nodes, "get ~p list for ~p path", [Group, URI]),
  {200, JSON} = eurl:get_page(URI, all),
  Wrapper = crowbar_rest:api_wrapper_raw(JSON),
  Wrapper#list.ids.
  
% DRY the path creation
group_node_path(Group, Node) ->  eurl:path([node:g(path), Node, "groups", Group]).

% Build Group JSON  
json(Name, Description, Order)           -> json(Name, Description, Order, "ui").
json(Name, Description, Order, Category) ->
  json:output([{"name",Name},{"description", Description}, {"category", Category}, {"order", Order}]).
	
% STEPS!
% TEMPORARY REMAPPING
step(_, In, Out) -> bdd_utils:depricated({2014, 1, 1}, group_cb, step, group_cb, step, [In, Out]).

step(_Given, {step_given, _N, ["REST adds the node",Node,"to",Group]}) -> 
  % TODO NOT TESTED
  step(_Given, {step_when, _N, ["REST adds the node",Node,"to",Group]});

step(_Given, {step_when, _N, ["REST adds the node",Node,"to",Group]}) -> 
  % TODO NOT TESTED
  bdd_utils:log(debug, group_cb, step, "group:step REST add the node ~p to ~p",[Node, Group]),
  eurl:put_post(group_node_path(Group, Node), [], put);

step(_Given, {step_when, _N, ["REST removes the node",Node,"from",Group]}) -> 
  % TODO NOT TESTED
  bdd_utils:log(debug, group_cb, step, "group:step REST remove the node ~p from ~p",[Node, Group]),
  eurl:delete(group_node_path(Group, Node), []);

step(_Result, {step_then, _N, ["there is not a",_Category,"group",Group]}) -> 
  % TODO NOT TESTED
  % WARNING - this IGNORES THE CATEGORY, it is not really a true test for the step.
  O = bdd_crud:read_obj(g(path), Group),
  case O#obj.id of
    "-1"  -> true;
    _    -> false
  end;

step(_Result, {step_then, _N, ["the group",Group,"should have at least",Count,"node"]}) -> 
  % TODO NOT TESTED
  {C, _} = string:to_integer(Count),
  Nodes = get_group_nodes(Group),
  bdd_utils:log(debug, group_cb, step_step, "Group ~p at least ~p from ~p", [Group, Count, Nodes]),
  Items = length(Nodes),
  Items >= C;

step(_Result, {step_then, _N, ["the group",Group,"should have",Count,"nodes"]}) -> 
  % TODO NOT TESTED
  {C, _} = string:to_integer(Count),
  Nodes = get_group_nodes(Group),
  bdd_utils:log(debug, group_cb, step_step, "Group ~p has ~p from ~p", [Group, Count, Nodes]),
  length(Nodes) =:= C;

step(_Result, {step_then, _N, ["the node",Node,"should be in group",Group]}) -> 
  % TODO NOT TESTED
  Nodes = get_group_nodes(Group),
  Name = [ N || {_ID, N} <- Nodes, N =:= Node],
  length(Name) =:= 1;

step(_Result, {step_then, _N, ["the node",Node,"should not be in group",Group]}) -> 
  % TODO NOT TESTED
  Nodes = get_group_nodes(Group),
  Name = [ N || {_ID, N} <- Nodes, N =:= Node],
  length(Name) =:= 0;

step(_Given, {step_finally, _N, ["REST removes the node",Node,"from",Group]}) -> 
  % TODO NOT TESTED
  step(_Given, {step_when, _N, ["REST removes the node",Node,"from",Group]});
                
step(_Global, {step_setup, _N, _}) -> 
  % create node(s) for tests
  %JSON0 = node:json(g(name_node1), g(description), 100),
  %Config0 = bdd_restrat:create(Config, node:g(path), g(atom_node1), name, JSON0),
  % create groups(s) for tests
  JSON1 = json(g(name1), g(description), 100),
  [_R1, O1] = bdd_crud:create(g(path), JSON1, g(atom1)),
  bdd_utils:log(debug, group_cb, step, "created obj  ~p with id ~p", [O1#obj.url, O1#obj.id]),
  JSON2 = json(g(name2), g(description), 200),
  [_R2, O2] = bdd_crud:create(g(path), JSON2, g(atom2)),
  bdd_utils:log(debug, group_cb, step, "created obj ~p with id ~p", [O2#obj.url, O2#obj.id]),
  true;

step(_Global, {step_teardown, _N, _}) -> 
  % find the node from setup and remove it
  bdd_crud:delete(g(atom2)),
  bdd_crud:delete(g(atom1)),
  true.
