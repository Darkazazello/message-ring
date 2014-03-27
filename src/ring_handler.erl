%%%-------------------------------------------------------------------
%%% @author Shelukhin Semyon <>
%%% @copyright (C) 2014, Shelukhin Semyon
%%% @doc
%%% Ringerman
%%% @end
%%% Created : 21 Mar 2014 by Shelukhin Semyon
%%%-------------------------------------------------------------------
-module('ring_handler').

-behaviour(gen_server).

-export([start_link/0, start/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {countProcess, nextPid, startTime, endTime}).
-include("../include/log.hrl").
-include_lib("../deps/mongrel/include/mongrel_macros.hrl").

start_link() ->
    gen_server:start_link(?MODULE, [], []).

start() ->
    gen_server:start_link(?MODULE, [], []).

init(_Args) ->
    {ok, #state{countProcess = 0, nextPid = self(), startTime = os:timestamp(), endTime = os:timestamp()}}.

handle_call(stop, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call({init,NextPid, N}, _From, _State) ->
    Reply = ok,
    {reply, Reply, #state{countProcess = N, nextPid = NextPid}}.

handle_cast({run, Current, M}, #state{nextPid=Pid, countProcess = N, startTime = StartTime, endTime = EndTime}) ->
    if
        Current == 0 ->
            StartTime_ = os:timestamp(),
            gen_server:cast(Pid, {run, Current + 1, M}),
            State = #state{nextPid=Pid,countProcess=N, startTime = StartTime_, endTime = EndTime},
            {noreply, State};
        Current == M * N ->
            EndTime_ = os:timestamp(),
            %%TODO add unlock in ets table
            write_to_log(Pid, N, M, StartTime, EndTime_),
            State = #state{nextPid=Pid,countProcess=N, startTime = StartTime, endTime = EndTime_},
            {noreply, State};
        true ->
            Event = {run,Current + 1, M},
            gen_server:cast(Pid, Event),
            State = #state{nextPid=Pid,countProcess=N, startTime = StartTime, endTime = EndTime},
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


write_to_log(Pid, N, M,StartTime, EndTime) ->
    Host = {localhost, 27017}, %%TODO move parametr to application env
    {ok, Conn} = mongo:connect(Host),
    Log = #log{?id(),pid=binary:list_to_bin(pid_to_list(Pid)), n=N, m=M, startTime=StartTime, endTime=EndTime},
    mongrel:do(safe, master, Conn, test, fun() -> mongrel:insert(Log) end).
