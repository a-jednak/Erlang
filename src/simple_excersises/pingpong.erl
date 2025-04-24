-module(pingpong).
-author("dell").

%% API
-export([start/0, stop/0, play/1, ping/0, pong/0]).

start() ->
  register(ping, spawn(pingpong, ping, [])),
  register(pong, spawn(pingpong,pong, [])),
  ok.

stop() ->
  ping ! stop,
  pong ! stop,
  ok.

play(N) ->
  ping ! N.
%  ping ! {play, N}.

ping() ->
  receive
    N when N > 0 ->
      timer:sleep(1000),
      io:format("Ping ~p~n", [N]),
      pong ! N-1,
      ping()
  after
    20000 ->
      stop()
  end.


pong() ->
  receive
    N when N > 0 ->
      timer:sleep(1000),
      io:format("Pong ~p~n", [N]),
      ping ! N-1,
      pong()
  after
    20000 ->
      stop()
  end.


