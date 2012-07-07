%% @doc Module for collecting metrics during test execution.

-module(perforator_metrics).

-compile(export_all).
-export([
    init_collect/0,
    retrieve/1,
    average/1
]).

-define(COLLECT_INTERVAL, 300).
-define(MAX_RETRIEVE_WAIT, ?COLLECT_INTERVAL*3).


init_collect() ->
    spawn_link(fun collector_process/0).

collector_process() ->
    Metrics = get_metrics(),
    Stats = [{perforator_utils:get_timestamp(), Metrics}],
    collector_process(Stats, ?COLLECT_INTERVAL).

collector_process(Stats, SleepTime) ->
    receive {retrieve, Sender} -> Sender ! {stats, Stats}
    after SleepTime ->
        MoreMetrics = get_metrics(),
        NewStats = [{erlang:now(), MoreMetrics}|Stats],
        collector_process(NewStats, SleepTime)
    end.

retrieve(Pid) ->
    Pid ! {retrieve, self()},
    receive
        {stats, Stats} ->
            {ok, Stats}
    after ?MAX_RETRIEVE_WAIT ->
        {error, unable_to_retrieve_stats}
    end.


get_metrics() ->
    [
        %% @todo make sure cpu_sup:util() is called just before starting the
        %% test run.
        {cpu_util, cpu_sup:util()},
        {cpu_load, cpu_load()}
    ] ++ mem_load().

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


%% ===================================================================
%% Helpers
%% ===================================================================


%% @doc Returns CPU load in standart form (i.e. like from top)
cpu_load() ->
    cpu_sup:avg1() / 256.

mem_load() ->
    MemLoad = memsup:get_system_memory_data(),
    UsedMem = proplists:get_value(total_memory, MemLoad) -
        proplists:get_value(free_memory, MemLoad),
    UsedSwp = proplists:get_value(total_swap, MemLoad) -
        proplists:get_value(free_swap, MemLoad),
    [{used_memory, UsedMem}, {used_swap, UsedSwp}].
