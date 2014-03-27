-module(welcome_page_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2, terminate/3]).
 
 
init({tcp, http}, Req, []) ->
  {ok, Req, undefined_state};
init({tcp, http}, Req, OnlyFile) ->
  {ok, Req, OnlyFile}.
 
handle(Req, undefined_state = State) ->
  send(Req, State);
 
handle(Req, _OnlyFile = State) ->
  send(Req, State).
 
send(Req, State) ->
  case file() of
    {ok, Body} ->
      Headers = [{<<"Content-Type">>, <<"text/plain">>}],
      {ok, Req2} = cowboy_http_req:reply(200, Headers, Body, Req),
      {ok, Req2, State};
    _ ->
      {ok, Req2} = cowboy_http_req:reply(404, [], <<"404'd">>, Req),
      {ok, Req2, State}
  end.
 
terminate(_Req, _State) ->
    ok.
terminate(_A, _B, _C) ->
    ok.

file() ->
  file:read_file("../priv/index.html").
