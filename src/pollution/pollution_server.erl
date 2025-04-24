-module(pollution_server).

-export([
  start/0,stop/0,init/0,
  show_monitor/0,
  add_station/2, add_value/4, remove_value/3,
  get_one_value/3, get_station_mean/2, get_daily_mean/2, get_over_limit/2]).


start() ->
  register(server, spawn(pollution_server, init, [])).

stop() ->
  server ! stop,
  unregister(server),
  ok.

init() ->
  loop([]).

loop(Monitor) ->
  receive
    stop ->
      ok;

    {get_monitor, PID} ->
      PID ! Monitor,
      loop(Monitor);

    {add_station, {Name, Coords}, PID} ->
      NewMonitor = pollution:add_station(Name, Coords, Monitor),
      case NewMonitor of
        {error, _} ->
%%          print_error(Message),
          PID ! NewMonitor,
          loop(Monitor);
        _ ->
          PID ! ok,
          loop(NewMonitor)
      end;

    {add_value, {Identity, DateTime, Type, Val}, PID} ->
      NewMonitor = pollution:add_value(Identity, DateTime, Type, Val, Monitor),
      case NewMonitor of
        {error, _} ->
          PID ! NewMonitor,
          loop(Monitor);
        [{error, _}] ->
%%          print_error(Message),
          PID ! lists:nth(1, NewMonitor),
          loop(Monitor);
        _ ->
          PID ! ok,
          loop(NewMonitor)
      end;

    {remove_value, {Identity, DateTime, Type}, PID} ->
      NewMonitor = pollution:remove_value(Identity, DateTime, Type, Monitor),
      case NewMonitor of
        {error, _} ->
%%          print_error(Message),
          PID ! NewMonitor,
          loop(Monitor);
        _ ->
          PID ! ok,
          loop(NewMonitor)
      end;

    {get_one_value, {Identity, Date, Type}, PID} ->
      Result = pollution:get_one_value(Identity, Date, Type, Monitor),
      case Result of
        {error, Message} ->
%%          print_error(Message),
          PID ! {error, Message};
        _ -> PID ! Result
      end,
      loop(Monitor);

    {get_station_mean, {Identity, Type}, PID} ->
      Result = pollution:get_station_mean(Identity, Type, Monitor),
      case Result of
        {error, Message} ->
%%          print_error(Message),
          PID ! {error, Message};
        _ -> PID ! Result
      end,
      loop(Monitor);

    {get_daily_mean, {Type, Date}, PID} ->
      Result = pollution:get_daily_mean(Type, Date, Monitor),
      case Result of
        {error, Message} ->
%%          print_error(Message),
          PID ! {error, Message};
        _ -> PID ! Result
      end,
      loop(Monitor);

    {get_over_limit, {Time, Type}, PID} ->
      Result = pollution:get_over_limit(Time, Type, Monitor),
      case Result of
        {error, Message} ->
%%          print_error(Message),
          PID ! {error, Message};
        _ -> PID ! Result
      end,
      loop(Monitor);

    _ -> io:format("Error: Unknown command"),
      loop(Monitor)
  end.


%%print_error(Message) ->
%%  io:format("Error: ~s~n", [Message]).

show_monitor() ->
  server ! {get_monitor, self()},
  receive
    Monitor -> Monitor
  end.


add_station(Name, Coords) ->
  server ! {add_station, {Name, Coords}, self()},
  receive
    Res -> Res
  end.

add_value(Identity, DateTime, Type, Val) ->
  server ! {add_value, {Identity, DateTime, Type, Val},self()},
  receive
    Res -> Res
  end.

remove_value(Identity, DateTime, Type) ->
  server ! {remove_value, {Identity, DateTime, Type}, self()},
  receive
    Res -> Res
  end.

get_one_value(Identity, Date, Type) ->
  server ! {get_one_value, {Identity, Date, Type}, self()}, % self() - wysyła własne PID i czeka na opdowiedź
  receive Result -> Result end.

get_station_mean(Identity, Type) ->
  server ! {get_station_mean, {Identity, Type}, self()},
  receive Result -> Result end.

get_daily_mean(Type, Date) ->
  server ! {get_daily_mean, {Type, Date}, self()},
  receive Result -> Result end.

get_over_limit(Time, Type) ->
  server ! {get_over_limit, {Time, Type}, self()},
  receive Result -> Result end.

