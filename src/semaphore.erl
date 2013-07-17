%% @doc: Performant semaphore implemented with ETS

-module(semaphore).
-include("semaphore.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([acquire/2, release/1]).
-export([count/1, reset/1, reset/2]).

acquire(Name, Max) ->
    case catch ets:update_counter(?TABLE, Name,
                                  [{2, 0}, {2, 1, Max, Max}]) of
        {'EXIT', {badarg, _}} ->
            true = ets:insert_new(?TABLE, {Name, 1}),
            ok;

        [Max, Max] ->
            {error, max_reached};

        [_Count, Max] ->
            ok
    end.

release(Name) ->
    case catch ets:update_counter(?TABLE, Name, {2, -1, 0, 0}) of
        {'EXIT', E} ->
            {error, E};
        _N ->
            ok
    end.

count(Name) ->
    case catch ets:lookup(?TABLE, Name) of
        [{Name, Count}] ->
            Count;
        [] ->
            0
    end.


reset(Name) ->
    reset(Name, 0).

reset(Name, Count) ->
    catch ets:update_element(?TABLE, Name, {2, Count}).

%%
%% TESTS
%%

semaphore_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(api()),
      ?_test(release())
     ]}.


setup() ->
    application:start(semaphore),
    ok.

teardown(_) ->
    application:stop(semaphore),
    ok.

api() ->
    reset(foo),
    ?assertEqual(0, count(foo)),
    ?assertEqual(ok, acquire(foo, 1)),
    ?assertEqual(1, count(foo)),
    ?assertEqual({error, max_reached}, acquire(foo, 1)),
    ?assertEqual(1, count(foo)),
    ?assertEqual(ok, release(foo)),
    ?assertEqual(0, count(foo)),
    ?assertEqual(ok, acquire(foo, 1)),
    ?assertEqual(1, count(foo)).

release() ->
    reset(foo),
    ?assertEqual(ok, acquire(foo, 1)),
    ?assertEqual(ok, release(foo)),
    ?assertEqual(ok, release(foo)),
    ?assertEqual(ok, release(foo)),
    ?assertEqual(ok, acquire(foo, 1)),
    ?assertEqual({error, max_reached}, acquire(foo, 1)),
    ?assertEqual(1, count(foo)).

    
