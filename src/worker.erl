-module(worker).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([code_change/3, init/1, handle_call/3, handle_info/2, terminate/2]).


start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  {ok, []}.

handle_call({perform, Uri, _Origin}, _From, Res) ->
  {Time, {ok, {{_Version, _200, _ReasonPhrase}, _Headers, _Body}}} = timer:tc(httpc, request, [Uri]),
  {reply, {done, Time}, Res};
handle_call(terminate, _From, Res) ->
  {stop, normal, ok, Res}.

handle_info(Msg, Res) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, Res}.

terminate(normal, _Res) ->
  io:format("Worker ~w terminating~n", [self()]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

stop() ->
  ok.
