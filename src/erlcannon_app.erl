-module(erlcannon_app).
-behaviour(application).

-export([start/2, stop/1]).
-export([request_generator/4, process_request/0]).

start(_StartType, _StartArgs) ->
  io:format("Started worker ~w~n", [self()]),
  application:start(inets),
  application:start(ssl),
  R1 = spawn(erlcannon_app, process_request, []),
  R2 = spawn(erlcannon_app, process_request, []),
  R3 = spawn(erlcannon_app, process_request, []),
  R4 = spawn(erlcannon_app, process_request, []),
  spawn(erlcannon_app, request_generator, [start, 50, [], R1]),
  spawn(erlcannon_app, request_generator, [start, 50, [], R2]),
  spawn(erlcannon_app, request_generator, [start, 50, [], R3]),
  spawn(erlcannon_app, request_generator, [start, 50, [], R4]),
  erlcannon_sup:start_link().

request_generator(0, Initial, Res, ProcessorPID) ->
  Min = lists:min(Res) / 1000,
  Max = lists:max(Res) / 1000,
  Avg = lists:sum(Res)/length(Res) / 1000,
  io:format("Finished ~w Requests~nMin: ~wms~nMax: ~wms~nAverage: ~wms~n",
    [Initial, Min, Max, Avg]),
  ProcessorPID ! finished;
request_generator(start, Initial, Res, ProcessorPID) ->
  ProcessorPID ! {"https://google.com", self()},
  receive
    {done, NewRes} ->
      request_generator(Initial-1, Initial, Res ++ [NewRes], ProcessorPID)
  end;
request_generator(Rem, Initial, Res, ProcessorPID) ->
  ProcessorPID ! {"https://google.com/", self()},
  receive
    {done, NewRes} ->
      request_generator(Rem-1, Initial, Res ++ [NewRes], ProcessorPID)
  end.

process_request() ->
  receive
    finished ->
      io:format("~w Finished!~n", [self()]);
    {Dest, Origin} ->
      {Time, {ok, {{_Version, _200, _ReasonPhrase}, _Headers, _Body}}} = timer:tc(httpc, request, [Dest]),
      Origin ! {done, Time},
      process_request()
  end.

stop(_State) ->
  ok.
