-module(run_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {M, Req3} = cowboy_req:qs_val(<<"m">>, Req2),
    {Id, Req4} = cowboy_req:qs_val(<<"id">>, Req3),
    
    {ok, Req5} = echo(Method, M, Id, Req4),
    {ok, Req5, State}.

echo(<<"GET">>, undefined, _,Req) ->
    cowboy_req:reply(400, [], <<"Missing M parameter.">>, Req);
echo(<<"GET">>, _, undefined,Req) ->
    cowboy_req:reply(400, [], <<"Missing Id parameter.">>, Req);
echo(<<"GET">>, M, Id, Req) ->
    {M_,Tail} = string:to_integer(M),
    if
        M_ == error ->
            cowboy_req:reply(400, [], <<"M parametr must be integer.">>, Req);
        Tail == [] ->
            run_ring(Id, M_), 
            cowboy_req:reply(200, [
                                   {<<"content-type">>, <<"text/plain; charset=utf-8">>}
	], Echo, Req);
         true ->
            cowboy_req:reply(400, [], <<"M parametr must be integer.">>, Req)
     end;   

echo(_, _, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) ->
	ok.

run_ring(Id, M) ->
    A = ets:match(pending, {Id,'$1', '_'}),
    ok.
