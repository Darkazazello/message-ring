-module(list_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("../include/ring.hrl").

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(_Req, _State) ->
    %StringConverted = lists:map(ConvertFun, get_rings()),
%    Output = mochijson2:encode(get_rings()),
    cowboy_req:reply(200, [
                           {<<"content-type">>, <<"application/json; charset=utf-8">>}
                          ], jsx:encode(get_rings()), _Req).

terminate(_Reason, _Req, _State) ->
	ok.

get_rings() ->
    ets:match(rings,#ring{id='$1', status=pending,supPid='_',workerPid='_',n='$2'}).

