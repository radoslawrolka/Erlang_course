-module('Caculator').
-author("Rad").

%% API
-export([number_of_readings/2,
         number_of_readings/3,
         calculate_max/2,
         calculate_max/3,
         calculate_mean/2,
         calculate_mean/4]).

%% READ: {NAME, {{YEAR, MONTH, DAY}, {HOUR, MIN, SEC}}, [{TYPE, VALUE}, ...]}

number_of_readings([], _) -> 0;
number_of_readings(List, Date) -> number_of_readings(List, Date, 0).

number_of_readings([], _, Acc) -> Acc;
number_of_readings([{_, {Date, _}, _}|Tail], Date, Acc) -> number_of_readings(Tail, Date, Acc+1);
number_of_readings([_|Tail], Date, Acc) -> number_of_readings(Tail, Date, Acc).


calculate_max([], _) -> 0;
calculate_max(List, Type) -> calculate_max(List, Type, 0).

calculate_max([], _, Max) -> Max;
calculate_max([{_, _, []}|ListTail], Type, Max) -> calculate_max(ListTail, Type, Max);
calculate_max([{_Name, _Date, [{Type, Value}|ReadTail]}|ListTail], Type, Max) -> calculate_max([{_Name, _Date, ReadTail}|ListTail], Type, max(Max, Value));
calculate_max([{_Name, _Date, [{_, _}|ReadTail]}|ListTail], Type, Max) -> calculate_max([{_Name, _Date, ReadTail}|ListTail], Type, Max).


calculate_mean([], _) -> 0;
calculate_mean(List, Type) -> calculate_mean(List, Type, 0, 0).

calculate_mean([], _, _, 0) -> 0;
calculate_mean([], _, Sum, Count) -> Sum / Count;
calculate_mean([{_Name, _Type, [{Type, Value}|ReadTail]}|ListTail], Type, Sum, Count) -> calculate_mean([{_Name, _Type, ReadTail}|ListTail], Type, Sum+Value, Count+1);
calculate_mean([{_Name, _Type, [{_, _}|ReadTail]}|ListTail], Type, Sum, Count) -> calculate_mean([{_Name, _Type, ReadTail}|ListTail], Type, Sum, Count);
calculate_mean([{_, _, []}|ListTail], Type, Sum, Count) -> calculate_mean(ListTail, Type, Sum, Count).

% L = [{"Name1", {{2024, 3, 7}, {12, 33, 0}}, [{"Type1", 1.0}, {"Type2", 2.0}]}, {"Name2", {{2024, 3, 7}, {12, 33, 0}}, [{"Type1", 3.0}, {"Type2", 4.0}]}, {"Name3", {{2024, 3, 7}, {12, 33, 0}}, [{"Type1", 5.0}, {"Type2", 6.0}]}].
% 'Caculator':number_of_readings(L, {2024, 3, 7}).
% 'Caculator':calculate_max(L, "Type1").
% 'Caculator':calculate_mean(L, "Type1").
