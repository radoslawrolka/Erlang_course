-module(pollution_value_collector_gen_statem).
-author("Rad").

-behaviour(gen_statem).

-export([init/1, callback_mode/0]).
-export([start_link/0, stop/0, terminate/3]).
-export([set_station/1, add_value/3, store_data/0]).
-export([idle/3, collect/3]).

start_link() -> gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_statem:stop(?MODULE).
terminate(_Reason, _State, _Data) -> ok.

init([]) -> {ok, idle, []}.
callback_mode() -> state_functions.

%% API
set_station(Name) -> gen_statem:cast(?MODULE, {setStation, Name}).
add_value(Date, Type, Value) -> gen_statem:cast(?MODULE, {addValue, Date, Type, Value}).
store_data() -> gen_statem:cast(?MODULE, flushData).

%% Handlers
%% idle - waiting for setStation event
%% collect - collecting data until flushData event

idle(_Event, {setStation, Name}, _State) ->
  case pollution:get_one_value(Name, 1, 1, 1) of
    {error, "Station not found"} -> {stop, "Station not found", []};
    _ -> {next_state, collect, {Name, []}}
  end.

collect(_Event, {addValue, Date, Type, Value}, {Name, Data}) ->
  {next_state, collect, {Name, [{Date, Type, Value} | Data]}};
collect(_Event, flushData, {Name, Data}) ->
  lists:foreach(fun({Date, Type, Value}) -> pollution_gen_server:addValue(Name, Date, Type, Value) end, Data),
  {next_state, idle, []}.
