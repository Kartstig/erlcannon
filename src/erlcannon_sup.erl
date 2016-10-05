-module(erlcannon_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  io:format("Supervisor ~w~n", [self()]),
  Procs = [],
  {ok, {{one_for_all, 1, 100}, Procs} }.
