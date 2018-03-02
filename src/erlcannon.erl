-module(erlcannon).
-behaviour(application).

-export([start/2, stop/1]).
-export([benchmark/2, request_generator/5]).

start(_StartType, _StartArgs) ->
  io:format("Started Erlcannon ~w~n", [self()]),
  application:start(inets),
  application:start(ssl),
  erlcannon_sup:start_link().

benchmark(Uri, Requests) ->
  {ok, R1} = worker:start_link(),
  io:format("Started worker ~w~n", [R1]),
  spawn(erlcannon, request_generator, [start, Uri, Requests, [], R1]).

request_generator(0, Uri, Initial, Res, ProcessorPID) ->
  Min = lists:min(Res) / 1000,
  Max = lists:max(Res) / 1000,
  Avg = lists:sum(Res)/length(Res) / 1000,
  io:format("Finished ~w Requests~nMin: ~wms~nMax: ~wms~nAverage: ~wms~n",
    [Initial, Min, Max, Avg]),
  gen_server:call(ProcessorPID, terminate);
request_generator(start, Uri, Initial, Res, ProcessorPID) ->
  {done, NewRes} = gen_server:call(ProcessorPID, {perform, Uri, self()}),
  request_generator(Initial-1, Uri, Initial, Res ++ [NewRes], ProcessorPID);
request_generator(Rem, Uri, Initial, Res, ProcessorPID) ->
  {done, NewRes} = gen_server:call(ProcessorPID, {perform, Uri, self()}),
  request_generator(Rem-1, Uri, Initial, Res ++ [NewRes], ProcessorPID).

stop(_State) ->
  ok.
