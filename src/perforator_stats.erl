%% @doc Various statistics functions
-module(perforator_stats).

-export([
    mean/1,
    average/1,
    min/1,
    max/1
]).

mean([]) ->
    0;
mean(List) ->
    lists:sum(List) / lists:length(List).

%% @doc Calculate mean values of every property given a list of proplists
%% e.g. average([[{a, 1}, {b, 10}], [{a, 3}, {b, 90}]]) -> [{a, 2}, {b, 50}].
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

%% @doc Calculate min values of every property given a list of proplists
%% e.g. average([[{a, 1}, {b, 10}], [{a, 3}, {b, 5}]]) -> [{a, 1}, {b, 5}].
min([FirstRead | Rest]) ->
    lists:foldl(
        fun(CurrentEntry, AccMin) ->
            [{Key, min(Val, proplists:get_value(Key, CurrentEntry))} ||
                {Key, Val} <- AccMin]
        end,
        FirstRead,
        Rest
    ).

%% @doc Calculate min values of every property given a list of proplists
%% e.g. average([[{a, 1}, {b, 10}], [{a, 3}, {b, 5}]]) -> [{a, 3}, {b, 10}].
max([FirstRead | Rest]) ->
    lists:foldl(
        fun(CurrentEntry, AccMin) ->
            [{Key, max(Val, proplists:get_value(Key, CurrentEntry))} ||
                {Key, Val} <- AccMin]
        end,
        FirstRead,
        Rest
    ).
