%%%-------------------------------------------------------------------
%%% @author Rad
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. kwi 2024 12:03
%%%-------------------------------------------------------------------
-module(sensor_dist).
-author("Rad").

%% API
-export([run1/0, count1/0, find_closest_parallel/2, run2/0, runw1/2, runw2/2, count2/0, compared/0]).


get_rand_locations(N) -> [{rand:uniform(10000), rand:uniform(10000)} || _ <- lists:seq(1, N)].

dist({X1, Y1}, {X2, Y2}) -> math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

find_for_person(PersonLocation, SensorsLocations) ->
  lists:foldl(fun(SensorLocation, {MinDist, MinSensor}) ->
    NewDist = dist(PersonLocation, SensorLocation),
    case NewDist < MinDist of
      true -> {NewDist, SensorLocation};
      false -> {MinDist, MinSensor}
    end
  end, {10000, {0, 0}}, SensorsLocations).

find_closest(PeopleLocations, SensorsLocations) -> lists:min([find_for_person(Person, SensorsLocations) || Person <- PeopleLocations]).

run1() ->
  People = get_rand_locations(20000),
  Scanners = get_rand_locations(1000),
  find_closest(People, Scanners).

count1() ->
  timer:tc(fun sensor_dist:run1/0, []).

find_for_person(PersonLocation, SensorsLocations, ParentPID) ->
  ParentPID ! find_for_person(PersonLocation, SensorsLocations).

find_closest_parallel(PeopleLocations, SensorsLocations) ->
  ParentPID = self(),
  [spawn(fun() -> find_for_person(Person, SensorsLocations, ParentPID) end) || Person <- PeopleLocations],
  lists:min([receive {Dist, _} -> Dist end || _ <- PeopleLocations]).

run2() ->
  People = get_rand_locations(20000),
  Scanners = get_rand_locations(1000),
  find_closest_parallel(People, Scanners).

count2() ->
  timer:tc(fun sensor_dist:run2/0, []).

runw1(P,L) -> find_closest(P,L).
runw2(P,L) -> find_closest_parallel(P,L).

compared() ->
  People = get_rand_locations(20000),
  Scanners = get_rand_locations(1000),
  {timer:tc(fun sensor_dist:runw1/2, [People, Scanners]), timer:tc(fun sensor_dist:runw2/2, [People, Scanners])}.