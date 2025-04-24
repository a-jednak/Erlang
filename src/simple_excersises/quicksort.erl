-module(quicksort).

-export([qs/1, random_elems/3, compare_speeds/3, less_than/2, grt_eq_than/2]).


less_than(List,Arg) ->
  [X || X <- List, X <Arg].

grt_eq_than(List, Arg) ->
  [X || X <- List, X >= Arg].

qs([]) -> [];
qs([Pivot|Tail]) ->
  qs( less_than(Tail,Pivot) ) ++ [Pivot] ++ qs( grt_eq_than(Tail,Pivot) ).

random_elems(N,Min,Max)->
  [rand:uniform(Max+1-Min)+Min-1 || _<-lists:seq(1,N)].

compare_speeds(List, Fun1, Fun2) ->
  {TimeA, _} = timer:tc(fun() -> Fun1(List) end),
  {TimeB, _} = timer:tc(fun() -> Fun2(List) end),
  io:format("Szybkosc pierwszej funkcji to: ~w μs~n", [TimeA]),
  io:format("Szybkosc drugiej funkcji to: ~w μs~n", [TimeB]).
