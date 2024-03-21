-module(main).
-author("Rad").

%% API
-export([power/2]).

power(_, 0) -> 1;
power(X, N) -> X * power(X, N-1).
