%%%-------------------------------------------------------------------
%%% @author aza <>
%%% @copyright (C) 2014, aza
%%% @doc
%%%
%%% @end
%%% Created : 22 Mar 2014 by aza <>
%%%-------------------------------------------------------------------
-module(ring_handler_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link(?MODULE, []).

init(_Args) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = temporary,
    Shutdown = brutal_kill,
    Type = worker,

    RingStart = {ring_handler, {ring_handler, start_link, []},
                 Restart, Shutdown, Type, [ring_handler]},
    {ok, {SupFlags, [RingStart]}}.

