-module(betaPollution).

-export([readings/0, number_of_readings/2, calculate_max/2, calculate_mean/2]).


%%%

get_date(List) ->
  [H|T] = List,
  if
    is_tuple(H) == true -> element(1,H);
    true -> get_date(T)
  end.

number_of_readings([], _) -> 0;
number_of_readings(Readings, Date) ->
  [H|T] = Readings,
  Reading = tuple_to_list(H),
  CurrDate = get_date(Reading),
  if
    CurrDate == Date -> 1 + number_of_readings(T, Date);
    true -> number_of_readings(T, Date)
  end.

%%%

of_type([], _) -> false;
of_type(Reading, Type) ->
  [H|T] = Reading,
  if
    element(1,H) == Type -> element(2,H);
    true -> of_type(T, Type)
  end.

calculate_max([], _) -> 0;
calculate_max(Readings, Type) ->
  check(Type),
  [H|T] = Readings,
  Reading = element(3,H),
  Val = of_type(Reading,Type),
  if
    Val /= false -> R = calculate_max(T, Type),
      if
        Val > R -> Val;
        true -> R
      end;
    true -> calculate_max(T, Type)
  end.

%%%

calculate_sum([], _) -> 0;
calculate_sum(Readings, Type) ->
  [H|T] = Readings,
  Val = of_type(element(3,H),Type),
  if
    Val /= false -> Val + calculate_sum(T, Type);
    true -> calculate_sum(T, Type)
  end.

number_of([], _) -> 0;
number_of(Readings, Type) ->
  [H|T] = Readings,
  Val = of_type(element(3,H),Type),
  if
    Val /= false -> 1 + number_of(T, Type);
    true -> number_of(T, Type)
  end.

calculate_mean(Readings, Type) ->
  check(Type),
  Counter = number_of(Readings, Type),
  Sum = calculate_sum(Readings, Type),
  Sum/Counter.

%%%

goodType(Type) ->
  case Type of
    "temperatura" -> true;
    "cisnienie" -> true;
    "wilgotnosc" -> true;
    "PM10" -> true;
    "PM25" -> true;
    _ -> false
  end.


check(Type) ->
  X = goodType(Type),
  if
    X == false -> error("Incorrect type!");
    true -> true
  end.

readings() ->
  [
    {
      "Centrum Informatyki",
      {
        %date,
        {3,3,2025},
        {17,51,0}
      },
      [
        %{readings},
        {"PM25", 30},
        {"PM10", 41},
        {"temperatura", 13.2}
      ]
    },
    {
      "Centrum Informatyki",
      {
        %date,
        {4,3,2025},
        {16,12,42}
      },
      [
        %{readings},
        {"PM25", 10},
        {"PM10", 19},
        {"temperatura", 13.8},
        {"wilgotnosc", 73}
      ]
    },
    {
      "Akropol",
      {
        {3,3,2025},
        {6,12,35}
      },
      [
        {"temperatura", 7.2}
      ]
    },
    {
      "Akropol",
      {
        {6,3,2025},
        {8,23,04}
      },
      [
        {"PM25", 17},
        {"temperatura", 15.4},
        {"wilgotnosc", 80},
        {"cisnienie", 1009.4}
      ]
    },
    {
      "Rondo Mogilskie",
      {
        {5,3,2025},
        {18,21,11}
      },
      [
        {"PM25", 37},
        {"PM10", 42},
        {"cisnienie", 1003.3},
        {"temperatura", 11.2}
      ]
    },
    {
      "Rondo Mogilskie",
      {
        {6,3,2025},
        {14,45,54}
      },
      [
        {"PM25", 34},
        {"PM10", 39},
        {"wilgotnosc", 82}
      ]
    }
  ].