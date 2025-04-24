-module(myLists).

-export([contains/2, duplicateElements/1, sumFloats/1, sumFloats/2]).

contains([], _) -> false;
contains(List, Val) ->
  [H|T] = List,
  if
    H == Val -> true;
    true -> contains(T, Val)
  end.

duplicateElements([]) -> [];
duplicateElements(List) ->
  [H|T] = List,
  NewList = [H, H | duplicateElements(T)],
  NewList.

sumFloats([]) -> 0;
sumFloats(List) ->
  [H|T] = List,
  if
    is_float(H) == true -> H + sumFloats(T);
    true -> sumFloats(T)
  end.

sumFloats([], Sum) -> Sum;
sumFloats(List, Sum) ->
  [H|T] = List,
  if
    is_float(H) == true -> sumFloats(T, Sum+H);
    true -> sumFloats(T, Sum)
  end.


