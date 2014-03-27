-module(run_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-export([run_ring/2]).
-include("../include/ring.hrl").

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
    M_ = to_int(M),
    Id_ = to_int(Id),
    if
        (M_ == error) or (Id_ == error) ->
            cowboy_req:reply(400, [], <<"Incorrect parameters.">>, Req);
        true ->
            run_ring(Id_,M_),
            cowboy_req:reply(200, [
                                   {<<"content-type">>, <<"text/plain; charset=utf-8">>}
                                  ], "ok", Req)
        end;
 
echo(_, _, _,Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) ->
	ok.

run_ring(Id, M) ->
    [[WorkerId|_]|_] = ets:match(rings, #ring{id=Id, workerPid = '$1', supPid = '_', status=pending, n='_'}),
    gen_server:cast(WorkerId, {run, 0, M}).
    
to_int(A) ->
    {A_,Tail} = string:to_integer(binary:bin_to_list(A)),
    if
        A_ == error ->
            error;
        Tail > [] ->
            error;
        true ->
            A_
    end.
