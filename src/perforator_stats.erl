%% @doc Various statistics functions
-module(perforator_stats).

-export([
    mean/1,
    average/1
]).

mean([]) ->
    0;
mean(List) ->
    lists:sum(List) / lists:length(List).

%% @doc Calculate mean values of every property given a list of proplists
%% e.g. avergage([[{a, 1}, {b, 10}], [{a, 3}, {b, 90}]) -> [{a, 2}, {b, 50}].
-spec average([[{atom(), integer() | float()}]]) -> [{atom(), float()}].
average(MetricsList) ->
    NumberOfReads = length(MetricsList),
    average(MetricsList, NumberOfReads, []).
average([FirstRead | Rest], NumberOfReads, []) ->
    average(Rest, NumberOfReads, FirstRead);
average([NextRead | Rest], NumberOfReads, Sum) ->
    NewSum =
        [{MetricTag, MetricVal + proplists:get_value(MetricTag, NextRead)} ||
        {MetricTag, MetricVal} <- Sum],
    average(Rest, NumberOfReads, NewSum);
average([], NumberOfReads, Sum) ->
    [{MetricTag, MetricVal / NumberOfReads} || {MetricTag, MetricVal} <- Sum].
