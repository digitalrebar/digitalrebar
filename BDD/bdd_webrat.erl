% Copyright 2013-4 Dell 
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
% 
-module(bdd_webrat).
-export([step/2, step/3]).
-include("bdd.hrl").

% helper routine
click_link(URL, Link) ->
	Result = case URL of
		[]  -> bdd_utils:log(warn, bdd_webrat, click_link, "CANNOT FIND LINK ~p", [Link]), error;
		_   -> eurl:get_http(URL)
	end,
	Result.

% DEPRICATE!
step(_Config, B, C) -> bdd_utils:depricate({2013, 12, 1}, bdd_restrat, step, bdd_restrat, step, [B, C]).

step(_Global, {step_given, _N, ["I am on the home page"]}) -> 
	eurl:get_http([]);

step(Global, {step_given, _N, ["I am on the", Page, "page"]}) ->
  step(Global, {step_given, _N, ["I went to the", Page, "page"]});
  
step(_Global, {step_given, _N, ["I went to the", Page, "page"]}) ->
	eurl:get_http(Page);

% use this with parameter Key is Value
step(_Given, {step_when, {Scenario, _N}, ["I am on the", Page, "page with parameter", Key]}) ->
  step(_Given, {step_given, {Scenario, _N}, ["I am on the", Page, "page with parameter", Key]});
step(_Global, {step_given, {Scenario, _N}, ["I am on the", Page, "page with parameter", Key]}) ->
  Param = bdd_utils:scenario_retrieve(Scenario, Key, ""),
  URL = Page ++ "?" ++ Key ++ "=" ++ Param,
  bdd_utils:log(debug, bdd_webrat, step, "Getting ~p for page ~p + ~p=~p",[URL, Page, Key, Param]),
  eurl:get_http(URL);
	
% use bdd:[module].[lookup][value]
step(_Global, {step_given, {Scenario, _N}, ["parameter",Key,"is",Value]}) ->
  bdd_utils:log(debug, bdd_webrat, step, "Store parameter ~p = ~p", [Key, Value]),
  bdd_utils:scenario_store(Scenario, Key, Value),
  [];

step(_Given, {step_when, _N, ["I go to the home page"]}) -> 
	eurl:get_http([]);

step(_Given, {step_when, _N, ["I go to the", Page, "page"]}) -> 
	eurl:get_http(Page);

step(_Given, {step_when, _N, ["I try to go to the", Page, "page"]}) ->
	eurl:get_http(Page);

step(_Given, {step_given, _N, ["I post", Path]}) ->
  step(_Given, {step_given, _N, ["I",post, Path]});
step(_Given, {step_given, _N, ["I put", Path]}) ->
  step(_Given, {step_given, _N, ["I",put, Path]});

step(_Given, {step_given, _N, ["I",Verb, Path]}) ->
  eurl:put_post(Path, "{}", Verb);

step(Given, {step_when, _N, ["I click on the",Link,"link"]}) ->
  G = eurl:get_result(Given, http, "text/html"),
  URL = eurl:find_link(Link, G),
	click_link(URL, Link);

step(Given, {step_when, _N, ["I click on the", Link, "link in section", Id]}) -> 
  Body = eurl:get_result(Given, http, "text/html"),
  Section = eurl:find_div(Body, Id),
  URL = eurl:find_link(Link, Section),
  click_link(URL, Link);

step(Given, {step_when, _N, ["I click on the", Menu, "menu item"]}) -> 
  G = eurl:get_result(Given, http, "text/html"),
  [Block] = eurl:find_block("<li", "</li>", G#http.data, ">"++Menu++"</a>", 250),
  URL = eurl:find_link(Menu, Block),
  click_link(URL, Menu);

step(_Given, {step_given, {_Scenario, _N}, ["I post", Fields, "to", URL]}) ->
  eurl:put_post(URL, json:output(Fields), post);

step(Given, {step_given, _N, ["I fill in", Fields, "and submit using the",ButtonText,"button"]}) ->
  % assume a single form element
  Form = eurl:find_form(Given, ButtonText),
  bdd_utils:log(debug, "bdd_webrat:step When I fill in is Given ~p using ~p",[Form, Fields]),
  {fields, FormFields} = lists:keyfind(fields, 1, Form),
  SubmitFields = {fields, [eurl:form_fields_merge(F, Fields) || F <- FormFields]},
  NewForm = lists:keyreplace(fields, 1, Form, SubmitFields),
  bdd_utils:log(debug, "bdd_webrat:step When I fill in submitting ~p",[NewForm]),
  eurl:form_submit(NewForm);
  
step(Result, {step_then, _N, ["I should not see", Text]}) -> 
	bdd_utils:log(trace, "step_then result ~p should NOT have ~p on the page", [Result, Text]),
	eurl:search(Text,Result, false);

step(Result, {step_then, {_Scenario, _N}, ["I should not see", Text, "in section", Id]}) -> 
	bdd_utils:log(debug, "step_then result ~p should NOT have ~p on the page", [Result, Text]),
  step(Result, {step_then, {_Scenario, _N}, ["I should see", Text, "in section", Id]}) =:= false;

step(Result, {step_then, {_Scenario, _N}, ["I should see a checked check box",Name]}) ->
  try find_checkbox(Result, Name) of
    {{match, _}, {match, _}} -> true;
    _ -> false
  catch
    _ -> false
  end;

step(Result, {step_then, {_Scenario, _N}, ["I should see an unchecked check box",Name]}) ->
  try find_checkbox(Result, Name) of
    {nomatch, {match, _}} -> true;
    _ -> false
  catch
    _ -> false
  end;

step(Result, {step_then, {_Scenario, _N}, ["I should see an input box with",Input]}) ->
  R = eurl:get_result(Result, http, "text/html"), 
  {ok, Rex} = re:compile("<input\ (.+?)value=\""++Input++"\"(.+?)>", [multiline, dotall, {newline , anycrlf}]),
  M = re:run(R#http.data, Rex),
  try M of
    {match, _} -> true;
    _ -> false
  catch
    _ -> false
  end;

step(Result, {step_then, {_Scenario, _N}, ["I should see an input box",ID,"with",Input]}) ->
  R = eurl:get_result(Result, http, "text/html"), 
  {ok, Rex} = re:compile("<input\ (.+?)id=\""++ID++"\"(.+?)value=\""++Input++"\"(.+?)>", [multiline, dotall, {newline , anycrlf}]),
  M = re:run(R#http.data, Rex),
  try M of
    {match, _} -> true;
    _ -> false
  catch
    _ -> false
  end;

step(Result, {step_then, {_Scenario, _N}, ["I should not see heading", Text]}) 
  -> step(Result, {step_then, _N, ["I should see heading", Text]}) =:= false;

step(Result, {step_then, N, ["I should see a heading", Text]}) -> 
  step(Result, {step_then, N, ["I should see heading", Text]});
step(Result, {step_then, _N, ["I should see heading", Text]}) -> 
	bdd_utils:log(debug, bdd_webrat, step, "see heading ~p", [Text]),
  R = eurl:get_result(Result, http, "text/html"),
	eurl:find_heading(R#http.data,Text);

step(Result, {step_then, _N, ["I should see", Text]}) -> 
  R = eurl:get_result(Result, http, "text/html"), 
	bdd_utils:log(trace, bdd_restrat, step, "I should see ~p on ~p", [Text, R#http.url]),
	eurl:search(Text,R);

step(Result, {step_then, _N, ["I should see", Text, "in section", Id]}) -> 
  bdd_utils:log(debug, bdd_restrat, step, "~p should be in ~p", [Text, Id]),  
  R = eurl:get_result(Result, http, "text/html"),
  bdd_utils:log(trace, bdd_restrat, step, "~p should have ~p on the page", [R#http.url, Text]),
	Body = R#http.data,
	Section = eurl:find_div(Body, Id),
	eurl:search(Text,[Section]);

step(Result, {step_then, _N, ["there are no i18n errors"]}) -> 
  step(Result, {step_then, _N, ["there should be no translation errors"]}); 
step(Result, {step_then, _N, ["there are no localization errors"]}) -> 
  step(Result, {step_then, _N, ["there should be no translation errors"]});
step(Result, {step_then, _N, ["there should be no translation errors"]}) -> 
  TransError = bdd_utils:config(translation_error),
  R = eurl:get_result(Result, http, "text/html"),
  eurl:search(TransError,R, false);


step(Result, {step_then, _N, ["I should see a link to", Link]}) ->
  bdd_utils:assert([ 
  	try eurl:find_link(Link,R) of
  		_ -> true
  	catch
  	  error: E -> bdd_utils:log(error, bdd_webrat, step, "Did not find link to ~p (Error: ~p)", [Link, E]), false
  	end
  	|| R <- Result]
	);
  
step(Result, {step_then, _N, ["I should see a button with", Button]}) -> 
  R = eurl:get_result(Result, http, "text/html"),
  try eurl:find_button(Button,R) of
    _ -> true
	catch
	  error: E -> bdd_utils:log(warn, bdd_webrat, step, "Did not find ~p in page (Error: ~p)", [Button, E]), false
	end;
	
step(Result, {step_then, _N, ["I should download a PDF"]}) -> 
  bdd_utils:assert([
    case string:substr(R, 1, 4) of 
      "%PDF" -> true;
      _ -> false
    end
    || R <- Result ]
    );

step(Result, {step_then, _N, ["I should download text with", Text]}) -> 
	RE = case re:compile(Text, [multiline, dotall, {newline , anycrlf}]) of
	  {ok, R} -> R;
	  Error -> io:format("ERROR: Could not parse regex: '~p'.", [Error]), []
	end,
	case re:run(Result, RE) of
	  {match, _} -> true;
	  _ -> false
	end;

step(Result, {step_then, _N, ["I should see a menu for", Menu]}) -> 
  R = eurl:get_result(Result),
  eurl:find_block("<li", "</li>", R#http.data, Menu) =/= [];

step(Result, {step_then, _N, ["we should get a 404 return"]}) -> 
  R = eurl:get_result(Result),
  bdd_utils:log(debug, bdd_webrat, step, "404 return had ~p", R),
  R#http.code =:= 404;

step(_Result, {_Type, _N, ["END OF WEBRAT"]}) ->
  false.

% utilities

% we need to check for BOTH checked and unchecked to make sure there IS a checkbox
% returns the TWO regex results: checkbox checked and checkbox withotu checked
find_checkbox(Result, Name) ->
  R = eurl:get_result(Result, http, "text/html"), 
  {ok, RexCheck} = re:compile("<input\ (.+?)checked=(.+?)(name|id)=\""++Name++"\"(.+?)type=\"checkbox\"(.+?)>", [multiline, dotall, {newline , anycrlf}]),
  {ok, RexUnCheck} = re:compile("<input\ (.+?)(name|id)=\""++Name++"\"(.+?)type=\"checkbox\"(.+?)>", [multiline, dotall, {newline , anycrlf}]),
  MCheck = re:run(R#http.data, RexCheck),
  bdd_utils:log(debug, bdd_webrat, find_checkbox, "Checkbox checked? ~p", [MCheck]),
  MUnCheck = re:run(R#http.data, RexUnCheck),
  bdd_utils:log(debug, bdd_webrat, find_checkbox, "Checkbox UNchecked? ~p", [MUnCheck]),
  {MCheck,MUnCheck}.

