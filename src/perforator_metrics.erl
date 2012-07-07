%% @doc Module for collecting metrics during test execution.

-module(perforator_metrics).

-compile(export_all).
-export([
    init_collect/0,
    retrieve/1,
    agregate/1
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
        NewStats = [{perforator_utils:get_timestamp(), MoreMetrics}|Stats],
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

-spec agregate([{atom(), integer() | float()}]) ->
[
    {average, [{atom(), integer() | float()}]} |
    {min, [{atom(), integer() | float()}]} |
    {max, [{atom(), integer() | float()}]}
].
agregate(PropList) ->
    [
        {average, perforator_stats:average(PropList)},
        {min, perforator_stats:min(PropList)},
        {max, perforator_stats:max(PropList)}
    ].

get_metrics() ->
    [
        %% @todo make sure cpu_sup:util() is called just before starting the
        %% test run.
        {cpu_util, cpu_sup:util()},
        {cpu_load, cpu_load()}
    ] ++ mem_load().

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
