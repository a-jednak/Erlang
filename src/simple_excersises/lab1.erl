-module(lab1).

-export([power/2]).

power(N, 1) -> N;
power(N, S) ->
  N*power(N,S-1).