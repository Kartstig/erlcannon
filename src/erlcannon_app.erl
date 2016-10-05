-module(erlcannon_app).
-behaviour(application).

-export([start/2, stop/1]).
-export([request_generator/3, process_request/0]).

start(_StartType, _StartArgs) ->
  io:format("Started worker ~w~n", [self()]),
  application:start(inets),
  application:start(ssl),
  R1 = spawn(erlcannon_app, process_request, []),
  R2 = spawn(erlcannon_app, process_request, []),
  R3 = spawn(erlcannon_app, process_request, []),
  R4 = spawn(erlcannon_app, process_request, []),
  spawn(erlcannon_app, request_generator, [start, 50, R1]),
  spawn(erlcannon_app, request_generator, [start, 50, R2]),
  spawn(erlcannon_app, request_generator, [start, 50, R3]),
  spawn(erlcannon_app, request_generator, [start, 50, R4]),
  erlcannon_sup:start_link().

request_generator(0, Initial, ProcessorPID) ->
  io:format("Finished ~w Requests~n", [Initial]),
  ProcessorPID ! finished;
request_generator(start, Initial, ProcessorPID) ->
  ProcessorPID ! {"https://google.com", self()},
  receive
    {done, Res} ->
      io:format("Got Result: ~w~n", [Res])
  end,
  request_generator(Initial-1, Initial, ProcessorPID);
request_generator(Rem, Initial, ProcessorPID) ->
  ProcessorPID ! {"https://www.google.com/", self()},
  receive
    {done, Res} ->
      io:format("Got Result: ~w~n", [Res])
  end,
  request_generator(Rem-1, Initial, ProcessorPID).

process_request() ->
  receive
    finished ->
      io:format("Finished!~n");
    {Dest, Origin} ->
      io:format("Handling ~s~n", [Dest]),
      {Time, {ok, {{_Version, 200, _ReasonPhrase}, _Headers, _Body}}} = timer:tc(httpc, request, [Dest]),
      Origin ! {done, Time},
      process_request()
  end.

stop(_State) ->
  ok.
