-module(create_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

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
    {N_,Tail} = string:to_integer(N),
    if
        N == error ->
            cowboy_req:reply(400, [], <<"N parametr must be integer.">>, Req);
        Tail == [] ->
            Res = create_ring(N),
            if
                Res == ok ->
                    cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], "ok", Req);
                true ->
                    cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], "failed", Req)
            end;
         true ->
            cowboy_req:reply(400, [], <<"N parametr must be integer.">>, Req)
     end;   

echo(_, _, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) ->
	ok.

create_ring(N) ->
    Res = supervisor:start_child(main_sup, []),
    if
        Res == {ok, SupPid} ->
            Pids = create_handlers(SupPid,N, []),
            ets:insert(rings, [{status, pending}, {supId, SupPid}, {n,N}]),
            if
                Pids == {error, _} ->
                    {error, "Create message ring failed"};
                true ->
                    [First | _] = Pids
                    init_ring(N, lists:append(Pids,First))
            end;
        
        true ->
            {error, "Create message ring failed"}
    end
        ok.
    
create_handlers(Pid,0, Pids) -> 
    Pids;
create_handlers(Pid, N, Pids) ->
    Res = supervisor:start_child(Pid, []),
    if
        Res == {ok, Pid} ->
            create_handlers(Pid, N - 1, lists:append(Pids, Pid));
        true ->
            {error, "Fail"}
    end.

init_ring(_, H|[]) ->
    ok;
init_ring(N, H|Tail) ->
    [NextPid | _] = Tail,
    gen_server:call(H, {init,NextPid,N}),
    init_ring(N, Tail).
   
