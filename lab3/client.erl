%%%-------------------------------------------------------------------
%%% @author Rad
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. kwi 2024 15:30
%%%-------------------------------------------------------------------
-module(client).
-author("Rad").

%% API
-export([start_server/0,
  stop_server/0,
  add_station/2,
  add_value/4,
  remove_value/3,
  get_one_value/3,
  get_station_mean/2,
  get_daily_mean/2,
  get_correlation/3]).

start_server() -> pollution_server:start().
stop_server() -> pollution_server:stop().
add_station(Name, Coords) -> pollution_server ! {addStation, {Name, Coords}}, ok.
add_value(StationID, DateTime, Type, Value) -> pollution_server ! {addValue, {StationID, DateTime, Type, Value}}, ok.
remove_value(StationID, DateTime, Type) -> pollution_server ! {removeValue, {StationID, DateTime, Type}}, ok.
get_one_value(StationID, DateTime, Type) -> pollution_server ! {getOneValue, {StationID, DateTime, Type}}, ok.
get_station_mean(StationID, Type) -> pollution_server ! {getStationMean, {StationID, Type}}, ok.
get_daily_mean(DateTime, Type) -> pollution_server ! {getDailyMean, {DateTime, Type}}, ok.
get_correlation(Type1, Type2, DateTime) -> pollution_server ! {getCorrelation, {Type1, Type2, DateTime}}, ok.

%%client:start_server().
%%client:add_station("Station1", {1, 2}).
%%client:add_value("Station1", "2024-04-06 15:30", "PM10", 50).
%%client:add_value("Station1", "2024-04-06 15:30", "PM2.5", 20).
%%client:get_one_value("Station1", "2024-04-06 15:30", "PM10").
%%client:get_station_mean("Station1", "PM10").
%%client:get_daily_mean("2024-04-06", "PM10").
%%client:get_correlation("PM10", "PM2.5", "2024-04-06 15:30").
%%client:stop_server().