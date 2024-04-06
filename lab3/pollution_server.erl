%%%-------------------------------------------------------------------
%%% @author Rad
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. kwi 2024 12:40
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Rad").

%% API
-export([start/0, stop/0]).

start() -> register(pollution_server, spawn(fun() -> init() end)).

stop() ->
  case whereis(pollution_server) of
    undefined -> io:format("Pollution server is not running~n", []);
    Pid -> Pid ! stop
  end.

init() ->
  io:format("Pollution server started~n", []),
  loop(pollution:create_monitor()).

loop(Monitor) ->
  receive
    {addStation, {Name, Coords}} ->
      io:format("Adding station~n", []),
      NewMonitor = pollution:add_station(Name, Coords, Monitor),
      case NewMonitor of
        {error, Reason} -> io:format("Error: ~p~n", [Reason]), loop(Monitor);
        _ -> io:format("Station added~n", []), loop(NewMonitor)
      end;
    {addValue, {StationID, DateTime, Type, Value}} ->
      io:format("Adding value~n", []),
      NewMonitor = pollution:add_value(StationID, DateTime, Type, Value, Monitor),
      case NewMonitor of
        {error, Reason} -> io:format("Error: ~p~n", [Reason]), loop(Monitor);
        _ -> io:format("Value added~n", []), loop(NewMonitor)
      end;
    {removeValue, {StationID, DateTime, Type}} ->
      io:format("Removing value~n", []),
      NewMonitor = pollution:remove_value(StationID, DateTime, Type, Monitor),
      case NewMonitor of
        {error, Reason} -> io:format("Error: ~p~n", [Reason]), loop(Monitor);
        _ -> io:format("Value removed~n", []), loop(NewMonitor)
      end;
    {getOneValue, {StationID, DateTime, Type}} ->
      io:format("Getting one value~n", []),
      Value = pollution:get_one_value(StationID, DateTime, Type, Monitor),
      case Value of
        {error, Reason} -> io:format("Error: ~p~n", [Reason]), loop(Monitor);
        _ -> io:format("Value: ~p~n", [Value]), loop(Monitor)
      end;
    {getStationMean, {StationID, Type}} ->
      io:format("Getting station mean~n", []),
      Mean = pollution:get_station_mean(StationID, Type, Monitor),
      case Mean of
        {error, Reason} -> io:format("Error: ~p~n", [Reason]), loop(Monitor);
        _ -> io:format("Mean: ~p~n", [Mean]), loop(Monitor)
      end;
    {getDailyMean, {DateTime, Type}} ->
      io:format("Getting daily mean~n", []),
      Mean = pollution:get_daily_mean(DateTime, Type, Monitor),
      case Mean of
        {error, Reason} -> io:format("Error: ~p~n", [Reason]), loop(Monitor);
        _ -> io:format("Mean: ~p~n", [Mean]), loop(Monitor)
      end;
    {getCorrelation, {StationID, Type1, Type2}} ->
      io:format("Getting correlation~n", []),
      Correlation = pollution:get_correlation(StationID, Type1, Type2, Monitor),
      case Correlation of
        {error, Reason} -> io:format("Error: ~p~n", [Reason]), loop(Monitor);
        _ -> io:format("Correlation: ~p~n", [Correlation]), loop(Monitor)
      end;
    stop -> io:format("Pollution server stopped~n", [])
  end.
