-module(semaphore_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
                {semaphore, {semaphore_server, start_link, []},
                 permanent, 5000, worker, []}
               ],
    {ok, {{one_for_one, 5, 10}, Children}}.

