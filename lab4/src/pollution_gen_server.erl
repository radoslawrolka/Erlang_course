%%%-------------------------------------------------------------------
%%% @author Rad
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(pollution_gen_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, terminate/2, code_change/3,
  addStation/2,
  addValue/4,
  removeValue/3,
  getOneValue/3,
  getStationMean/2,
  getDailyMean/2,
  getCorrelation/3,
  crash/0]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================

addStation(Name, Coords) -> gen_server:call(?MODULE, {addStation, {Name, Coords}}).
addValue(StationID, DateTime, Type, Value) -> gen_server:call(?MODULE, {addValue, {StationID, DateTime, Type, Value}}).
removeValue(StationID, DateTime, Type) -> gen_server:call(?MODULE, {removeValue, {StationID, DateTime, Type}}).
getOneValue(StationID, DateTime, Type) -> gen_server:call(?MODULE, {getOneValue, {StationID, DateTime, Type}}).
getStationMean(StationID, Type) -> gen_server:call(?MODULE, {getStationMean, {StationID, Type}}).
getDailyMean(DateTime, Type) -> gen_server:call(?MODULE, {getDailyMean, {DateTime, Type}}).
getCorrelation(StationID, Type1, Type2) -> gen_server:call(?MODULE, {getCorrelation, {StationID, Type1, Type2}}).

crash() -> gen_server:call(?MODULE, crash).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, pollution:create_monitor()}.

handle_call({addStation, {Name, Coords}}, _From, State) ->
   case pollution:add_station(Name, Coords, State) of
     {error, Reason} -> {reply, {error, Reason}, State};
     NewState -> {reply, ok, NewState}
   end;
handle_call({addValue, {StationID, DateTime, Type, Value}}, _From, State) ->
  case pollution:add_value(StationID, DateTime, Type, Value, State) of
    {error, Reason} -> {reply, {error, Reason}, State};
    NewState -> {reply, ok, NewState}
  end;
handle_call({removeValue, {StationID, DateTime, Type}}, _From, State) ->
  case pollution:remove_value(StationID, DateTime, Type, State) of
    {error, Reason} -> {reply, {error, Reason}, State};
    NewState -> {reply, ok, NewState}
  end;
handle_call({getOneValue, {StationID, DateTime, Type}}, _From, State) ->
  case pollution:get_one_value(StationID, DateTime, Type, State) of
    {ok, Value} -> {reply, {ok, Value}, State};
    {error, Reason} -> {reply, {error, Reason}, State}
  end;
handle_call({getStationMean, {StationID, Type}}, _From, State) ->
  case pollution:get_station_mean(StationID, Type, State) of
    {error, Reason} -> {reply, {error, Reason}, State};
    Mean -> {reply, {ok, Mean}, State}
  end;
handle_call({getDailyMean, {DateTime, Type}}, _From, State) ->
  case pollution:get_daily_mean(DateTime, Type, State) of
    {error, Reason} -> {reply, {error, Reason}, State};
    Mean -> {reply, {ok, Mean}, State}
  end;
handle_call({getCorrelation, {StationID, Type1, Type2}}, _From, State) ->
  case pollution:get_correlation(StationID, Type1, Type2, State) of
    {error, Reason} -> {reply, {error, Reason}, State};
    Correlation -> {reply, {ok, Correlation}, State}
  end.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
