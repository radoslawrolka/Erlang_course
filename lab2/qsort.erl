%%%-------------------------------------------------------------------
%%% @author Rad
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. mar 2024 11:42
%%%-------------------------------------------------------------------
-module(qsort).
-author("Rad").

%% API
-export([less_than/2, grt_eq_than/2, qs/1, random_elems/3, compare_speeds/3]).

less_than(List, Arg) -> [X || X <- List, X < Arg].

grt_eq_than(List, Arg) -> [X || X <- List, X >= Arg].

qs([]) -> [];
qs([Pivot|Tail]) -> qs(less_than(Tail, Pivot)) ++ [Pivot] ++ qs(grt_eq_than(Tail, Pivot)).

random_elems(N, Max, Min) -> [rand:uniform(Max - Min) + Min || _ <- lists:seq(1, N)].

compare_speeds(List, Fun1, Fun2) ->
  {Time1, _} = timer:tc(Fun1, [List]),
  {Time2, _} = timer:tc(Fun2, [List]),
  io:format("Time1: ~p, Time2: ~p~n", [Time1, Time2]).

% qsort:compare_speeds(qsort:random_elems(1000, 10, 0), fun lists:sort/1, fun qsort:qs/1).