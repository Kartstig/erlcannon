-module(worker).
-behaviour(gen_server).

-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, start_link/1, terminate/2]).
-export([results/1]).

-include("worker_state.hrl").


% API
-spec results(term()) -> #worker_state{}.
results(Name) ->
  gen_server:call(Name, {results}).


% gen_server

start_link(Name) ->
  gen_server:start_link({local, Name}, ?MODULE, [], []).

init([]) ->
  {ok, #worker_state{}}.

handle_cast({perform, Uri}, State) ->
  case timer:tc(httpc, request, [Uri]) of
    {Time, {ok, {{_Version, _200, _ReasonPhrase}, _Headers, _Body}}} ->
      NewState = State#worker_state{
        requests=(State#worker_state.requests + 1),
        time=(State#worker_state.time + Time),
        resp_200=(State#worker_state.resp_200 + 1)
        },
      {noreply, NewState};
    {Time, {error, {_Reason,_}}} ->
      NewState = State#worker_state{
        requests=(State#worker_state.requests + 1),
        time=(State#worker_state.time + Time)
        },
      {noreply, NewState};
    BadResult ->
      io:format("Error:~n~p~n", [BadResult]),
      {noreply, State}
  end.

handle_call(results, _From, State) ->
  {reply, State, State};
handle_call(terminate, _From, State) ->
  {stop, normal, ok, State}.

handle_info(Msg, State) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, State}.

terminate(normal, _State) ->
  io:format("Worker ~w terminating~n", [self()]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
