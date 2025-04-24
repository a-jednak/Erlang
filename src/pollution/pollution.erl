
-module(pollution).
-author("dell").

-export([create_monitor/0, add_station/3, add_value/5, remove_value/4,get_one_value/4,
  get_station_mean/3, get_daily_mean/3, get_over_limit/3]).
-record(reading, {type, val, time}).
-record(station, {name, coords, readings}).


create_monitor() -> [].


add_station(Name, Coordinates, Monitor) ->
  case [X || X <- Monitor, X#station.name == Name orelse X#station.coords == Coordinates] of
    [] -> Monitor ++ [#station{name = Name, coords = Coordinates, readings = []}];
    _ -> {error,"Station with this name/coordinates already exists!"}
  end.


add_value(Identity, DateTime, Type, Val, Monitor) ->
  NewReading = #reading{type = Type, val = Val, time = DateTime},
  case has_station(Identity, Monitor) of
    {error, X} -> {error, X};
    _ -> [if
            (Station#station.name == Identity) orelse (Station#station.coords == Identity) -> %sprawdzanie czy istnieje stacja
              case [Reading || Reading <- Station#station.readings,
                Reading#reading.type == Type, Reading#reading.time == DateTime] of %lista readingow ze stacji o tym samym typie i czasie
                [] -> Station#station{readings = Station#station.readings ++ [NewReading]}; %jesli ich nie ma to spk dodajemy
                _ -> {error,"Another reading at this time and station already exists!"} %jesli jakies sa to error
              end;
            true -> Station
          end || Station <- Monitor]
  end.

remove_value(Identity, Date, Type, Monitor) ->
  case has_station(Identity, Monitor) of
    {error, X} -> {error, X};
    {ok, Station} ->
      case [Reading || Reading <- Station#station.readings,
            Reading#reading.type == Type, Reading#reading.time == Date] of
        [] -> {error, "Reading does not exist."};
        _ ->
          UpdatedStation = Station#station{
            readings = [Reading || Reading <- Station#station.readings,
              not (Reading#reading.type == Type andalso Reading#reading.time == Date)]
          },

          [if
             S#station.name == Identity orelse S#station.coords == Identity ->
               UpdatedStation;
             true -> S
           end || S <- Monitor]
      end
  end.


get_one_value(Identity, Date, Type, Monitor) ->
  case has_station(Identity, Monitor) of
    {error, X} -> {error, X};
    {ok, Station} ->
      MatchingReadings = [Reading || Reading <- Station#station.readings,
        Reading#reading.time == Date,
        Reading#reading.type == Type],
      case MatchingReadings of
        [] -> {error, "There is no reading of this type and time."};
        [Reading | _] -> Reading#reading.val
      end
  end.


get_station_mean(Identity, Type, Monitor) ->
  case has_station(Identity, Monitor) of
    {error, X} -> {error, X};
    {ok, Station} ->
      Filtered = [Reading#reading.val || Reading <- Station#station.readings, Reading#reading.type == Type],
      case Filtered of
        [] -> {error, "No readings of type that day in station."};
        _ -> lists:sum(Filtered)/length(Filtered)
      end
  end.


get_daily_mean(Type, Date, Monitor) ->
  Filtered = lists:flatten([[Reading#reading.val
      || Reading <- Station#station.readings,
      Reading#reading.type == Type,
      element(1,Reading#reading.time) == Date]
    || Station <- Monitor
  ]),
  case Filtered of
    [] -> {error, "No readings of type that day"};
    _ -> lists:sum(Filtered)/length(Filtered)
  end.


% Dodatkowe
get_over_limit(Time, Type, Monitor) ->
  Limit = case Type of
    "PM10" -> 50;
    "PM25" -> 30;
    _ -> {error,"This type does not exist or has no stated limit."}
  end,
  case Limit of
    {error, _} -> Limit;
    _ -> lists:sum([
      case [Reading#reading.val
        || Reading <- Station#station.readings,
        element(1, element(2, Reading#reading.time)) == Time,
        Reading#reading.type == Type,
        Reading#reading.val > Limit] of
        [] -> 0;
        _ -> 1
      end
      || Station <- Monitor
    ])
  end.


has_station(Identity, Monitor) ->
  MatchingStations = [Station || Station <- Monitor,
    Station#station.name == Identity orelse Station#station.coords == Identity],
  case MatchingStations of
    [] -> {error, "Station not found."};
    [Station] -> {ok, Station}
  end.


