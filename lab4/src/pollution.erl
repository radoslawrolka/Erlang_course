%%%-------------------------------------------------------------------
%%% @author Rad
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. mar 2024 12:32
%%%-------------------------------------------------------------------
-module(pollution).
-author("Rad").

%% API
-export([create_monitor/0, %  tworzy i zwraca nowy monitor zanieczyszczeń
  add_station/3, % dodaje do monitora wpis o nowej stacji pomiarowej (nazwa i współrzędne geograficzne), zwraca zaktualizowany monitor
  add_value/5, % dodaje odczyt ze stacji (współrzędne geograficzne lub nazwa stacji, data, typ pomiaru, wartość), zwraca zaktualizowany monitor
  remove_value/4, % usuwa odczyt ze stacji (współrzędne geograficzne lub nazwa stacji, data, typ pomiaru), zwraca zaktualizowany monitor
  get_one_value/4, % zwraca wartość pomiaru z zadanej stacji o zadanym typie i z zadanej daty
  get_station_mean/3, % zwraca średnią wartość parametru z zadanej stacji i danego typu;
  get_daily_mean/3, % zwraca średnią wartość parametru danego typu, danego dnia na wszystkich stacjach
  get_correlation/4]). % zwraca odchylenie standardowe z różnic pomiarów dwóch typów zanieczyszczeń na zadanej stacji

-record(monitor, {stations_by_name=#{}, stations_by_coords=#{}}).
-record(station, {name, coords, data=#{}}).
-record(measurement, {date_time, type, value}).


%%--------------------------------------------------------------------
create_monitor() -> #monitor{}.

%%--------------------------------------------------------------------
add_station(Name, Coords, Monitor) ->
  case {is_station_name_unique(Name, Monitor), is_station_coords_unique(Coords, Monitor)} of
    {false, _} -> {error, "Station name already exists"};
    {_, false} -> {error, "Station coords already exist"};
    _ ->
      NewStation = #station{name = Name, coords = Coords},
      NewMonitor = Monitor#monitor{stations_by_name = maps:put(Name, NewStation, Monitor#monitor.stations_by_name),
                                   stations_by_coords = maps:put(Coords, NewStation, Monitor#monitor.stations_by_coords)},
    NewMonitor
  end.

%%--------------------------------------------------------------------
add_value(StationID, DateTime, Type, Value, Monitor) ->
  case identify_station(StationID, Monitor) of
    {error, Reason} -> {error, Reason};
    {ok, Station} -> add_value_to_station(Station, DateTime, Type, Value, Monitor)
  end.
add_value_to_station(Station, DateTime, Type, Value, Monitor) ->
  case is_measurement_unique(Station#station.data, DateTime, Type) of
    false -> {error, "Measurement already exists"};
    true ->
      NewMeasurement = #measurement{date_time = DateTime, type = Type, value = Value},
      StationData = maps:put({DateTime, Type}, NewMeasurement, Station#station.data),
      NewStation = Station#station{data = StationData},
      NewMonitor = Monitor#monitor{stations_by_name = maps:put(Station#station.name, NewStation, Monitor#monitor.stations_by_name),
                                   stations_by_coords = maps:put(Station#station.coords, NewStation, Monitor#monitor.stations_by_coords)},
      NewMonitor
  end.

%%--------------------------------------------------------------------
remove_value(StationID, DateTime, Type, Monitor) ->
  case identify_station(StationID, Monitor) of
    {error, Reason} -> {error, Reason};
    {ok, Station} -> remove_value_from_station(Station, DateTime, Type, Monitor)
  end.
remove_value_from_station(Station, DateTime, Type, Monitor) ->
  case maps:is_key({DateTime, Type}, Station#station.data) of
    false -> {error, "Measurement not found"};
    true ->
      StationData = maps:remove({DateTime, Type}, Station#station.data),
      NewStation = Station#station{data = StationData},
      NewMonitor = Monitor#monitor{stations_by_name = maps:put(Station#station.name, NewStation, Monitor#monitor.stations_by_name),
                                   stations_by_coords = maps:put(Station#station.coords, NewStation, Monitor#monitor.stations_by_coords)},
      NewMonitor
  end.

%%--------------------------------------------------------------------
get_one_value(StationID, DateTime, Type, Monitor) ->
  case identify_station(StationID, Monitor) of
    {error, Reason} -> {error, Reason};
    {ok, Station} -> get_one_value_from_station(Station, DateTime, Type)
  end.
get_one_value_from_station(Station, DateTime, Type) ->
  case maps:is_key({DateTime, Type}, Station#station.data) of
    false -> {error, "Measurement not found"};
    true -> (maps:get({DateTime, Type}, Station#station.data))#measurement.value
  end.

%%--------------------------------------------------------------------
get_station_mean(StationID, Type, Monitor) ->
  case identify_station(StationID, Monitor) of
    {error, Reason} -> {error, Reason};
    {ok, Station} -> get_station_mean_from_station(Station, Type)
  end.
get_station_mean_from_station(Station, Type) ->
  Measurements = maps:values(Station#station.data),
  MeasurementsOfType = [M#measurement.value || M <- Measurements, M#measurement.type =:= Type],
  case length(MeasurementsOfType) of
    0 -> {error, "No measurements of this type"};
    _ -> lists:sum(MeasurementsOfType) / length(MeasurementsOfType)
  end.

%%--------------------------------------------------------------------
get_daily_mean(Type, Date, Monitor) ->
  Measurements = [M#measurement.value ||
    Station <- maps:values(Monitor#monitor.stations_by_coords),
    M <- maps:values(Station#station.data),
    M#measurement.type == Type,
    element(1, M#measurement.date_time) == Date],

  case length(Measurements) of
    0 -> {error, "No measurements of this type"};
    _ -> lists:sum(Measurements) / length(Measurements)
  end.


%%%-------------------------------------------------------------------
get_correlation(StationID, Type1, Type2, Monitor) ->
  case identify_station(StationID, Monitor) of
    {error, Reason} -> {error, Reason};
    {ok, Station} -> get_correlation_from_station(Station, Type1, Type2)
  end.
get_correlation_from_station(Station, Type1, Type2) ->
  Measurements = maps:values(Station#station.data),
  MeasurementsOfType1 = [M#measurement.value || M <- Measurements, Type1 == M#measurement.type],
  MeasurementsOfType2 = [M#measurement.value || M <- Measurements, Type2 == M#measurement.type],
  case {length(MeasurementsOfType1), length(MeasurementsOfType2)} of
    {0, _} -> {error, "No measurements of type 1"};
    {_, 0} -> {error, "No measurements of type 2"};
    _ ->
      Mean1 = lists:sum(MeasurementsOfType1) / length(MeasurementsOfType1),
      Mean2 = lists:sum(MeasurementsOfType2) / length(MeasurementsOfType2),
      Diff1 = [X - Mean1 || X <- MeasurementsOfType1],
      Diff2 = [X - Mean2 || X <- MeasurementsOfType2],
      Diff1Diff2 = lists:zip(Diff1, Diff2),
      Diff1Diff2Squared = [X1 * X2 || {X1, X2} <- Diff1Diff2],
      Covariance = lists:sum(Diff1Diff2Squared),
      StdDev1 = math:sqrt(lists:sum([X * X || X <- Diff1])),
      StdDev2 = math:sqrt(lists:sum([X * X || X <- Diff2])),
      case StdDev1 * StdDev2 of
        +0.0 -> +0.0;
        _ -> Covariance / (StdDev1 * StdDev2)
      end
  end.



% add_station utils
is_station_name_unique(Name, Monitor) -> not maps:is_key(Name, Monitor#monitor.stations_by_name).
is_station_coords_unique(Coords, Monitor) -> not maps:is_key(Coords, Monitor#monitor.stations_by_coords).

% add_value utils
identify_station(StationID, Monitor) ->
  case StationID of
    {_,_} -> get_station(StationID, Monitor#monitor.stations_by_coords);
    [_|_]  -> get_station(StationID, Monitor#monitor.stations_by_name);
     _ -> {error, "Invalid station ID"}
  end.
get_station(StationID, Map) ->
  case maps:is_key(StationID, Map) of
    true -> {ok, maps:get(StationID, Map)};
    false -> {error, "Station not found"}
  end.
is_measurement_unique(Data, DateTime, Type) -> not maps:is_key({DateTime, Type}, Data).
