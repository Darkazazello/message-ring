-module(list_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    cowboy_req:reply(200, [
                           {<<"content-type">>, <<"application/json; charset=utf-8">>}
                          ], get_rings(), Req).

terminate(_Reason, _Req, _State) ->
	ok.

get_rings() ->
    ets:match(rings,{'$1', '$2', '$3', pending}).
