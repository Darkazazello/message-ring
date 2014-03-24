%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(hello_world_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
    initialEts(),
    Dispatch = cowboy_router:compile([
                                      {'_', [
                                             {"/create", create_handler, []},
                                             {"/run", run_handler, []},
                                             {"/list", list_handler, []},
                                             {"/log", log_handler,[]}
                                            ]}
                                     ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [{env, [{dispatch, Dispatch}]}]),
    hello_world_sup:start_link(),
    main_sup:start_link().

stop(_State) ->
	ok.


initialEts() ->
    ets:new(rings, [bag,named_table,public]).
