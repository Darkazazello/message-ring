-module(log_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-export([get_log/0]).
-include("../include/log.hrl").
-include_lib("../deps/mongrel/include/mongrel_macros.hrl").

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
    {ok, Conn} = mongo:connect(Host),
    {ok, List} = mongrel:do(safe, master, Conn, test,
               fun() ->
                       Cursor = mongrel:find(#log{m= {'$gt', 0}}),
                       mongrel_cursor:rest(Cursor)
               end),
    R= io_lib:format("~p",[List]),
    lists:flatten(R).

   
