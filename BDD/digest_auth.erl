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

-module(digest_auth).

%%
%% Exported Functions
%%
-export([request/5, request/2, header/2, test_calc_response/0]).

%% Does digest authentication. 
%% Callback function passes an authorization header and a URL,
%% to make it possible to construct consequent http request with proper authorization.
%%
%% WINDOWS USERS: You should copy the OpenSSH lib & dll files matching you x86/64 os into the erl.exe directory!
%%
%% For example:
%% application:start(crypto).
%% application:start(inets).
%% Config = [{user,"crowbar"},{password,"crowbar"}].
%% digest_auth:request(Config, get, {URL, []}, [], []).
%%
%% Note: You can save time if you add the header to the Config:
%% ConfigPlus = digest_auth:header(Config, URL). 
%% digest_auth:request(ConfigPlus, get, {URL, []}, [], []).
%%
%% If your site is NOT digest, then this is basically a pass through with minimal overhead
%%

request(Config, URL) ->
  request(Config, get, {URL, [], [], []}, [], []).

request(Config, get, {URL, Header}, HTTPOptions, Options) ->
  request(Config, get, {URL, Header, [], []}, HTTPOptions, Options);

request(Config, delete, {URL}, HTTPOptions, Options) ->
  request(Config, delete, {URL, [], [], []}, HTTPOptions, Options);
  
request(Config, Method, {URL, Header, Type, Body}, HTTPOptions, Options) ->
  % prepare information that's common
  bdd_utils:debug("URL request ~p~n", [http_uri:parse(URL)]),
  {http, _, _Host, _Port, DigestURI, Params} = http_uri:parse(URL),
  User = proplists:get_value(user, Config),
  MethodStr = string:to_upper(atom_to_list(Method)),
  Password = proplists:get_value(password, Config),
  % if we have a header fields, add them to the header so we can avoid the round trip
  TrialHeader = case proplists:get_value(digest_field, Config) of 
    undefined -> Header;
    FieldsCache ->  
      HeaderInjection = buildAuthHeader(DigestURI++Params, MethodStr, User, Password, FieldsCache),
      Header ++ [{"Authorization", HeaderInjection}]
  end,
  % try request
  {Status,{{Protocol,Code,Comment}, Fields, Message}} = case Method of
    get -> httpc:request(Method, {URL, TrialHeader}, HTTPOptions, Options);
    delete -> httpc:request(Method, {URL, TrialHeader}, HTTPOptions, Options);
    _ -> httpc:request(Method, {URL, TrialHeader, Type, Body}, HTTPOptions, Options)
  end,
  % if 401, then get the auth info and retry (to save this, use the header/2 method to save the fields)
  case Code of
    401 -> 
      DigestLine = proplists:get_value("www-authenticate", Fields),
    	HeaderDigested = case DigestLine of
	      [$D, $i, $g, $e, $s, $t, $  | _] -> 
	              AuthHeader = buildAuthHeader(DigestURI++Params, MethodStr, User, Password, DigestLine),
	              Header ++ [{"Authorization", AuthHeader}];
	      [$B, $a, $s, $i, $c, $ | _] -> Header;
	      S -> "ERROR, unexpected digest header (" ++ S ++ ") should be Digest or Basic."
	    end,
      case Method of 
        get -> httpc:request(Method, {URL, HeaderDigested}, HTTPOptions, Options);
        delete -> httpc:request(Method, {URL, HeaderDigested}, HTTPOptions, Options);
        _ -> httpc:request(Method, {URL, HeaderDigested, Type, Body}, HTTPOptions, Options)
      end;
    _ -> {Status,{{Protocol,Code,Comment}, Fields, Message}}
  end.

%% Simplifed version of request that returns the Auth Header to save future round trips  
header(Config, URL) ->
  {Status,{{_Protocol,Code,_Comment}, Fields, _Message}} = httpc:request(URL++"/digest"),
  % if 401, then get the auth info and retry
  case {Status, Code} of
    {ok, 401} -> Config ++ [{digest_field, proplists:get_value("www-authenticate", Fields)}];
    _ -> Config
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
	HA1 = 	hex(binary_to_list(crypto:hash( string:join([User, Realm, Password], ":")))),
	HA2 = 	hex(binary_to_list(crypto:hash( string:join([Method, URI], ":")))),
	%io:format("HA1:~p~n", [HA1]),
	%io:format("HA2:~p~n", [HA2]),	
	%HA1 result, server nonce (nonce), request counter (nc), client nonce (cnonce), quality of protection code (qop) and HA2 result is calculated.
	Step3Arg = string:join([HA1, Nonce, Nc, CNonce, Qop, HA2], ":"),
  %io:format("3rd step:~p~n", [Step3Arg]),
	hex(binary_to_list(crypto:hash( Step3Arg))).

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



