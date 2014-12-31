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
-module(bdd_print).
-export([file/0, html/0, report/1, result/1, fail/1]).

file() -> bdd_utils:config(results_out,"/tmp/bdd_results.out").

html() ->
  HTML = bdd_utils:config(coverage_out,"/tmp/bdd.html"),
  {ok, [{test, Date, Time, Results} | _]} = file:consult(file()),
  {ok, S} = file:open(HTML, write),
  io:format(S, "<html>~n<header>BDD Test ~p at ~p</header>~n",[Date, Time]),
  io:format(S, "<body>~n<h1>BDD Test ~p at ~p</h1>~n",[Date, Time]),
  io:format(S, "<table border=1>~n",[]),
  io:format(S, "<tr><th>Feature</th><th>Description</th><th>Passed</th><th>Failed</th><th>Skipped</th><th>Total</th><th>%</th></tr>~n",[]),
  html_row(S, Results),  
  io:format(S, "~n</body>", []),
  io:format(S, "~n</html>", []),
  file:close(S).
  
html_row(File, [])                 -> io:format(File, "~n</table>", []);
html_row(File, [Result | Results]) ->
  {feature, Fatom, Feature, R} = Result,
  {Total, Pass, Fail, Skip, _Detail} = report(R),
  io:format(File, "<tr><td>~p</td><td>~s</td><td>~p</td><td>~p</td><td>~p</td><td>~p</td><td>~p</td></tr>~n", [Fatom, Feature, Pass, Fail, Skip, Total, round((Pass/Total)*100)]),
  html_row(File, Results).

report({feature, _, _, Result}) ->  report(Result);
report(Result)  ->
  Out = result(Result),
  {total, Total} = lists:keyfind(total, 1, Out),
  {pass, Pass, _P} = lists:keyfind(pass, 1, Out),
  {fail, Fail, IDs} = lists:keyfind(fail, 1, Out),
  {skip, Skip} = lists:keyfind(skip, 1, Out),
  {Total, Pass, Fail, Skip, IDs}.
result(Result)                  ->  result(Result, [], [], []).
result([], Pass, Fail, Skip)    ->  
  [{total, length(Pass)+length(Fail)+length(Skip)}, 
    {pass, length(Pass), Pass}, 
    {fail, length(Fail), Fail},
    {skip, length(Skip)}];
result([Result | T], Pass, Fail, Skip)->
  case Result of
    {ID, pass} -> F=Fail, P=[ID | Pass], S=Skip;
    {ID, skip} -> F=Fail, P=Pass, S=[ID | Skip];
    ok         -> P=Pass, F=Fail, S=[skip | Skip];
    {ID, _}    -> P=Pass, F=[ID | Fail], S=Skip
  end,
  result(T, P, F, S).
  
% output results information
fail([]) -> true;
fail({true, {_Type, N, Description}}) ->
  bdd_utils:log(step_pass,"~p: ~s", [N, lists:flatten([D ++ " " || D <- Description, is_list(D)])]);
fail({_, {_Type, N, Description}}) ->
  bdd_utils:log(step_fail,"~p: ~s", [N, lists:flatten([D ++ " " || D <- Description, is_list(D)])]);
fail([Result | Results]) ->
  fail(Result),
  fail(Results).

  
