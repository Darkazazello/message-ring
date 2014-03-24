-module(log_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, _State) ->

    cowboy_req:reply(200, [
                           {<<"content-type">>, <<"application/json; charset=utf-8">>}
                          ], get_log(), Req).

terminate(_Reason, _Req, _State) ->
	ok.

get_log() ->
    Host = {localhost, 27017},
    {ok, Conn} = mongo:connect (Host),
    mongo:do (safe, master, Conn, test, fun() -> mongo:find(log) end).
   
