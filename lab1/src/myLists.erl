%%%-------------------------------------------------------------------
%%% @author Rad
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. mar 2024 12:33
%%%-------------------------------------------------------------------
-module(myLists).
-author("Rad").

%% API
-export([contains/2,
  duplicateElements/1,
  sumFloats/2,
  sumFloats/1]).

contains(_, []) -> false;
contains(X, [X|_]) -> true;
contains(X, [_|T]) -> contains(X, T).

duplicateElements([]) -> [];
duplicateElements([H|T]) -> [H,H|duplicateElements(T)].

sumFloats([], Acc) -> Acc;
sumFloats([H|T], Acc) when is_float(H) -> sumFloats(T, Acc+H);
sumFloats([_|T], Acc) -> sumFloats(T, Acc).

sumFloats(L) -> sumFloats(L, 0.0).
