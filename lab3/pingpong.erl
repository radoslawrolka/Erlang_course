%%%-------------------------------------------------------------------
%%% @author Rad
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. kwi 2024 11:40
%%%-------------------------------------------------------------------
-module(pingpong).
-author("Rad").

%% API
-export([start/0, stop/0, play/1]).

-define(TIMEOUT, 20000).

start() ->
  register(ping, spawn(fun() -> ping_loop(0) end)),
  register(pong, spawn(fun() -> pong_loop(0) end)).

stop() ->
  ping ! kill,
  pong ! kill.

play(N) ->
  ping ! N.

ping_loop(State) ->
  receive
    kill -> ok;
    N when N =< 0 -> stop();
    N ->
      io:format("Ping received ~p, sumofmes ~p~n", [N, State+1]),
      pong ! N-1,
      ping_loop(State+1)
  after
    ?TIMEOUT -> io:format("Ping timeout~n")
  end.

pong_loop(State) ->
  receive
    kill -> ok;
    N when N =< 0 -> stop();
    N ->
      io:format("Pong received ~p, sumofmess ~p~n", [N, State+1]),
      ping ! N-1,
      pong_loop(State+1)
  after
    ?TIMEOUT -> io:format("Pong timeout~n")
  end.