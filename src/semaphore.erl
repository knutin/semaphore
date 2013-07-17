%% @doc: Performant semaphore implemented with ETS

-module(semaphore).
-include("semaphore.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([acquire/2, release/1]).

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
    case catch ets:update_counter(?TABLE, Name, -1) of
        {'EXIT', E} ->
            {error, E};
        _N ->
            ok
    end.



%%
%% TESTS
%%

semaphore_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(api())
     ]}.


setup() ->
    application:start(semaphore),
    ok.

teardown(_) ->
    application:stop(semaphore),
    ok.

api() ->
    ?assertEqual(ok, acquire(foo, 1)),
    ?assertEqual({error, max_reached}, acquire(foo, 1)),
    ?assertEqual(ok, release(foo)),
    ?assertEqual(ok, acquire(foo, 1)).
