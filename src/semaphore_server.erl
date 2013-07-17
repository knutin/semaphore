%% @doc: Owner of ets-table
-module(semaphore_server).
-behaviour(gen_server).
-include("semaphore.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

%%
%% API
%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%
%% GEN SERVER IMPLEMENTATION
%%

init([]) ->
    ?TABLE = ets:new(?TABLE, [named_table, public, set, {read_concurrency, true}]),
    {ok, #state{}}.

handle_call(_, _, State) ->
    {reply, unknown, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% INTERNAL FUNCTIONS
%%
