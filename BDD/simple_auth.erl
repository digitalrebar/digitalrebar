%% Original Author: bokner (boris.okner@gmail.com)
%% Created: Apr 10, 2010
%% Description: HTTP digest authentication
%% Note: the code follows particular explanation given on Wikipedia.
%% Disclaimer: Use on your own risk. The author disclaims any liability 
%% with regard to using this code.
%% Posted at git://gist.github.com/362131.git

%% Second Author: Rob Hirschfeld (@Zehicle)
%% Updated: Sept 19. 2011
%% Addendum Copyright 2011, Dell 
%% 
%% Licensed under the Apache License, Version 2.0 (the "License"); 
%% you may not use this file except in compliance with the License. 
%% You may obtain a copy of the License at 
%% 
%%  http://www.apache.org/licenses/LICENSE-2.0 
%% 
%% Unless required by applicable law or agreed to in writing, software 
%% distributed under the License is distributed on an "AS IS" BASIS, 
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
%% See the License for the specific language governing permissions and 
%% limitations under the License. 
%% 

-module(simple_auth).

%%
%% Exported Functions
%%
-export([request/4, request/1, authenticate_session/1, header/1, test_calc_response/0]).
-include("bdd.hrl").

%% Does digest authentication. 
%% Callback function passes an authorization header and a URL,
%% to make it possible to construct consequent http request with proper authorization.
%%
%% WINDOWS USERS: You should copy the OpenSSH lib & dll files matching you x86/64 os into the erl.exe directory!
%%
%% For example:
%% application:start(crypto).
%% application:start(inets).
%% digest_auth:request(Config, get, {URL, []}, [], []).
%%
%% Note: You can save time if you add the header to the Config:
%% ConfigPlus = digest_auth:header(Config, URL). 
%% digest_auth:request(ConfigPlus, get, {URL, []}, [], []).
%%
%% If your site is NOT digest, then this is basically a pass through with minimal overhead
%%
%% Here are the things you can get back from httpc:request. You get #1 or #2 depending on
%% 'full_result' option in Options array. Default is #1 (full_result => true)
%%
%%   ok, {{http_version(), status_code(), reason_phrase()}, headers, body}
%%   ok, {status_code(), body}
%%   error, {connect_failed, term}
%%   error, {send_failed, term}
%%   error, term
  
request_action(Method, {URL, Headers, ContentType, Body}, HTTPOptions, Options) ->
  %% prepare information that's common
  {http, _, _Host, Port, DigestURI, Params} = case http_uri:parse(URL) of
    {error, no_scheme} -> bdd_utils:log(error, simple_auth, request, "incomplete URL (needs http): ~p",[URL]);
    {ok, X} -> X;   % needed for newer erlang BIF
    X -> X
  end,
  User = bdd_utils:config(user),
  MethodStr = string:to_upper(atom_to_list(Method)),
  Password = bdd_utils:config(password),

  %% suppress auto-redirect - we have to manage it ourselves because 
  %% incorrectly drops port number off of redirect URL
  HTTPOptions2 = HTTPOptions ++ [{autoredirect, false}],

  %% if we have authentication data, add auth header to the headers 
  AuthField = bdd_utils:config(auth_field, undefined),
  DigestIndex = case AuthField of
    undefined -> 0;
    _ -> string:str(AuthField,"Digest")
  end,
  TrialHeaders = case AuthField of 
    undefined -> Headers; %++ [{"Accept", "text/html,*/*;q=0.0"}];
    FieldsCache when DigestIndex > 0 -> 
      HeaderInjection = buildAuthHeader(DigestURI++Params, MethodStr, User, Password, FieldsCache),
      Headers ++ [{"Authorization", HeaderInjection}];
    _ -> Headers ++ [{"Cookie", AuthField}]
  end,
  bdd_utils:log(dump, simple_auth, request, "making http request Method ~p URL ~p Headers ~p Opts ~p", [Method, URL, TrialHeaders, HTTPOptions2]),

  %% try request
  {Status, Result} = request(Method, URL, TrialHeaders, ContentType, Body, HTTPOptions2, Options),
  %% detect system error
  {{HTTPVersion, StatusCode, ReasonPhrase}, ResponseHeaders, ResponseBody} = 
    case Status of
      ok -> 
        Result;
      error ->  % for now, we'll return empty result 999 status
        {{"HTTP 1.0", 999, Result}, [], ""}
    end,

  bdd_utils:log(trace, simple_auth, request, "User ~p Password ~p URL ~p StatusCode ~p",[User, Password, URL, StatusCode]),
  case StatusCode of
    401 -> 
      bdd_utils:log(trace,  simple_auth, request, "URL ~p session did not auth.  This may be OK.  Falling back to digest.",[URL]),
      DigestLine = proplists:get_value("www-authenticate", ResponseHeaders),
      HeaderDigested = case DigestLine of
        [$D, $i, $g, $e, $s, $t, $  | _] -> 
          AuthHeader = buildAuthHeader(DigestURI++Params, MethodStr, User, Password, DigestLine),
          Headers ++ [{"Authorization", AuthHeader}];
        [$B, $a, $s, $i, $c, $ | _] -> Headers;
	      S -> "ERROR, unexpected digest header (" ++ S ++ ") should be Digest or Basic."
      end,
      request(Method, URL, HeaderDigested, ContentType, Body, HTTPOptions2, Options);

    302 ->
      % we have to shoehorn the port number back into the redirect URL - erlang bug?
      Location = proplists:get_value("location", ResponseHeaders),
      {http, _, NHost, _Port, NURI, _Params} = case http_uri:parse(Location) of
	{ok, {http, A, B, C, D, E}} -> {http, A, B, C, D, E};
        {http, A, B, C, D, E} -> {http, A, B, C, D, E}
      end,
      CorrectURL = assemble_url(NHost,Port,NURI),
      request(Method, CorrectURL, TrialHeaders, ContentType, Body, HTTPOptions2, Options);

    999 ->  % we have a system error such as cannot connect or timeout
      {Status,Result};

    _ -> {Status,{{HTTPVersion, StatusCode, ReasonPhrase}, ResponseHeaders, ResponseBody}}
  end.
request(Method, URL, Headers, ContentType, Body, HTTPOptions, Options) ->
  case Method of 
    get -> httpc:request(Method, {URL, Headers}, HTTPOptions, Options);
    delete -> httpc:request(Method, {URL, Headers}, HTTPOptions, Options);
    _ -> httpc:request(Method, {URL, Headers, ContentType, Body}, HTTPOptions, Options) 
  end.

% returns the HTTP record instead of just raw header information
request(URL)   -> request(get, URL, [], []).
request(Method, {URL, Headers, ContentType, Input}, HTTPOptions, Options) -> 
  % use the old response that returns the HTTPC tuple
  bdd_utils:log(trace, simple_auth, request, "~p to ~p", [Method, URL]),
  Response = request_action(Method, {URL, Headers, ContentType, Input}, HTTPOptions, Options),
  {ok, {{"HTTP/1.1",Code,_State}, Header, Body}} = Response,
  MediaType = proplists:get_value("content-type", Header),
  {DataType, Version} = case string:tokens(MediaType, ";=") of
    [D, " version", V | _ ] -> {D, V};
    [D | _ ]                -> {D, "0.0"};
    _                       -> {"unknown", "0.0"}
  end,                
  DT = string:tokens(DataType, ".+"),
  {Namespace, Details} = case DT of
    ["application/vnd", N | L]    -> {list_to_atom(N), L};
    ["application/json"]          -> {bdd_restrat, []};    % use bdd_utils get_object for json
    ["unknown"]                   -> {bdd_utils, []};      % default
    _                             -> {bdd_utils, []}       % fall back (same as default)
  end,
  bdd_utils:log(trace, simple_auth, request, "~p returned ~p (~p)", [URL, Code, MediaType]),
  bdd_utils:log(dump, simple_auth, request, "~p > ~p", [URL, Body]),
  % componse the object back together
  #http{data=Body, code=Code, url=URL, version=Version, datatype=DataType, namespace=Namespace, details=Details };

request(Method, URL, HTTPOptions, Options) -> request(Method, {URL, [], [], []}, HTTPOptions, Options).


assemble_url(Host,Port,Path) ->
  "http:"++"//"++Host++":"++integer_to_list(Port)++Path .


%% Authenticate and save session_id in config for use by all subsequent test steps
header(URL) -> 
  bdd_utils:log(depricate, "simple_auth:header should be replaced with authenticate_session",[]),
  authenticate_session(URL).
authenticate_session(URL) ->
  % we'll retry for about a minute to give server time to rev-up
  authenticate_session(URL, 20).
authenticate_session(_URL, 0) ->
  % we've retried out hearts out, time to bail...
  bdd_utils:config_set(auth_error, "could not connect to host");
authenticate_session(URL, Retries) ->
  User = bdd_utils:config(user),
  Password = bdd_utils:config(password),
  Result = httpc:request(post, {
			  URL++bdd_utils:config(sign_in_url, "/my/users/sign_in"),
			  [], 
			  "application/x-www-form-urlencoded",
			  "user[username]="++User++"&user[password]="++Password
			 },
			[], []),

  case Result of
    {Status,{{_Protocol,Code,_Comment}, Fields, _Message}} ->
      case {Status, Code} of
        {ok, 302} -> 
	  bdd_utils:log(info,"Authenticated as user: ~s",[User]),
	  Cookie = proplists:get_value("set-cookie", Fields),
          case Cookie of
            undefined -> % no session id returned!
              bdd_utils:config_set(auth_error, "Could not authenticate "++User++"/"++Password);
            _ -> % we should really parse the cookies, l8r
              Sessionidlen = string:str(Cookie, "; "),
              Sessionid = string:substr(Cookie,1,Sessionidlen),
              bdd_utils:config_set(auth_field, Sessionid)
          end;
        _ -> % did not successfully authenticate?
          bdd_utils:config_set(auth_error, "Could not authenticate "++User++"/"++Password)  
      end;

    _ -> % probably could not connect to host, try again!
	 timer:sleep(3000),
	 authenticate_session(URL, Retries-1)
  end.


buildAuthHeader(URI, Method, User, Password, DigestLine) ->
	buildMetaTag(calcResponse(DigestLine, User, Password, URI, Method, "0000000000000000")).

buildMetaTag(Components) ->
  {User, Realm, Nonce,  URI, Nc, CNonce, Response, Opaque} = Components,
	lists:flatten(io_lib:format("Digest username=\"~s\",realm=\"~s\",nonce=\"~s\",uri=\"~s\",qop=auth,nc=~s,cnonce=\"~s\",response=\"~s\",opaque=\"~s\"", [User, Realm, Nonce, URI,  Nc, CNonce, Response, Opaque])).

calcResponse(DigestLine, User, Password, URI, Method, Nc) ->
	random:seed(now()),
	[$D, $i, $g, $e, $s, $t, $  | DigestParamsStr] = DigestLine,
	DigestParams = [ realm_key(R, []) || R <- string:tokens(DigestParamsStr,",")],
	%% Calculate digest
	Realm = proplists:get_value("realm", DigestParams),
	Opaque = proplists:get_value("opaque", DigestParams),
	Nonce = proplists:get_value("nonce", DigestParams),
	CNonce = hex(integer_to_list(erlang:trunc(random:uniform()*10000000000000000))),
	Qop = proplists:get_value("qop", DigestParams),
	Response = calc_response(Method, User, Password, URI, Realm, Opaque, Nonce, Nc, CNonce, Qop),
	{User, Realm, Nonce,  URI, Nc, CNonce, Response, Opaque}.	

calc_response(Method, User, Password, URI, Realm, _Opaque, Nonce, Nc, CNonce, Qop) ->	
	HA1 = 	hex(binary_to_list(crypto:md5( string:join([User, Realm, Password], ":")))),
	HA2 = 	hex(binary_to_list(crypto:md5( string:join([Method, URI], ":")))),
	%io:format("HA1:~p~n", [HA1]),
	%io:format("HA2:~p~n", [HA2]),	
	%HA1 result, server nonce (nonce), request counter (nc), client nonce (cnonce), quality of protection code (qop) and HA2 result is calculated.
	Step3Arg = string:join([HA1, Nonce, Nc, CNonce, Qop, HA2], ":"),
  %io:format("3rd step:~p~n", [Step3Arg]),
	hex(binary_to_list(crypto:md5( Step3Arg))).

%% Implements example of digest response calculation from Wikipedia 
%% (http://en.wikipedia.org/wiki/Digest_access_authentication)
%% 
test_calc_response() ->
	crypto:start(),
	io:format("Proper response is: 6629fae49393a05397450978507c4ef1~n"),
	calc_response("GET", "Mufasa", "Circle Of Life", "/dir/index.html", "testrealm@host.com", "5ccc069c403ebaf9f0171e9517f40e41", "dcd98b7102dd2f0e8b11d0f600bfb0c093", "00000001","0a4f113b", "auth").

%%
%% Local Functions
%%

%% @hidden

realm_key([$  | Realm], [])   -> realm_key( Realm, []);
realm_key([$= | Realm], Key)  -> {Key, string:strip(Realm, both, $")};
realm_key([H | Realm], Key)   -> realm_key( Realm, Key++[H] ).


digit_to_xchar(D) when (D >= 0) and (D < 10) ->
	D + 48;
digit_to_xchar(D) ->
	D + 87.


hex(S) ->
	hex(S, []).



hex([], Res) ->
	lists:reverse(Res);
hex([N | Ns], Res) ->
	hex(Ns, [digit_to_xchar(N rem 16),
					 digit_to_xchar(N div 16) | Res]).
