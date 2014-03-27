-module(create_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-export([create_ring/1, create_handlers/3,init_ring/2]).
-include("../include/ring.hrl").
init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {N, Req3} = cowboy_req:qs_val(<<"n">>, Req2),
    {ok, Req4} = echo(Method, N, Req3),
    {ok, Req4, State}.

echo(<<"GET">>, undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing n parameter.">>, Req);
echo(<<"GET">>, N, Req) ->
    {N_,Tail} = string:to_integer(binary:bin_to_list(N)),
    if
        N_ == error ->
            cowboy_req:reply(400, [], <<"Incorrect n parameter.">>, Req);
        Tail > [] ->
            cowboy_req:reply(400, [], <<"Incorrect n parameter.">>, Req);
        true ->
            Res = create_ring(N_),
            if
                Res == ok ->
                    cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], "ok", Req);
                true ->
                    cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], "failed", Req)
            end
    end;

echo(_, _, Req) ->
	cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) ->
	ok.

create_ring(N) ->
    Res = supervisor:start_child(main_sup, []),
    if
         erlang:element(1, Res)== ok ->
            {ok, SupPid} = Res,
            Pids = create_handlers(SupPid,N, []),
            if
                error == Pids ->
                    {error, "Create message ring failed"};
                true ->
                    [First | _] = Pids,
                    {_,_,Id} = os:timestamp(),
                    ets:insert(rings, #ring{id=Id, status=pending, supPid = SupPid, workerPid = First, n=N}),
                    init_ring(N, lists:append(Pids,[First]))
            end;
        true ->
            {error, "Create message ring failed"}
    end.
    
create_handlers(_Pid, _N, Pids) when _N == 0 -> 
    Pids;

create_handlers(Pid, N, Pids) ->
    Res = supervisor:start_child(Pid, []),
    if
        erlang:element(1, Res) == ok  ->
            {ok, WorkerPid} = Res,
            NewPids = lists:append(Pids, [WorkerPid]),
            NewN = N - 1,
            create_handlers(Pid, NewN, NewPids);
        true ->
            error
    end.

init_ring(_, [_H|[]]) ->
    ok;
init_ring(N, [H|Tail]) ->
    [NextPid | _] = Tail,
    gen_server:call(H, {init,NextPid,N}),
    init_ring(N, Tail).
   
