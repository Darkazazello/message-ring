%%%-------------------------------------------------------------------
%%% @author aza <>
%%% @copyright (C) 2014, aza
%%% @doc
%%%
%%% @end
%%% Created : 22 Mar 2014 by aza <>
%%%-------------------------------------------------------------------
-module(main_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init(_Args) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = temporary,
    Shutdown = infinity,
    Type = supervisor,

    ringSupStart = {ring_handler_suo, {ring_handler_sup, start_link, []},
                 Restart, Shutdown, Type, [ring_handler_sup]},
    {ok, {SupFlags, [ringSupStart]}}.
