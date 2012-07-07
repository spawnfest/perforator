-module(perforator_metrics_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% Unit Test descriptions
%% ============================================================================

perforator_metrics_test_() ->
    {foreach,
        fun() ->
            application:load(sasl),
            application:set_env(sasl, errlog_type, error),
            application:start(sasl),
            application:start(os_mon)
        end,
        fun(_) ->
            application:stop(os_mon),
            application:stop(sasl)
         end,
        [
            {"Average value calculation test", fun test_average/0},
            {"Metrics lookup test", fun test_metrics/0},
            {"Collector process test", fun test_collector/0},
            {"Dying collect process test", fun test_dead_collector/0}
        ]
    }.

%% ============================================================================
%% Actual tests
%% ============================================================================

test_average() ->
    Reads = [
        [{cpu_util, N}, {cpu_load, N+3}] || N <- lists:seq(1, 3)
    ],
    ExpectedAverage = [{cpu_util, 2.0}, {cpu_load, 5.0}],
    ?assertEqual(ExpectedAverage, perforator_metrics:average(Reads)).

test_metrics() ->
    Read = perforator_metrics:get_metrics(),
    Check1 = [is_float(proplists:get_value(Metric, Read))
        || Metric <- [cpu_util, cpu_load]],
    Check2 = [is_integer(proplists:get_value(Metric, Read))
        || Metric <- [used_memory, used_swap]],
    ?assertEqual(lists:duplicate(length(Check1) + length(Check2), true),
        Check1 ++ Check2).

test_collector() ->
    Pid = perforator_metrics:init_collect(),
    timer:sleep(1000),
    {ok, Return} = perforator_metrics:retrieve(Pid),
    ?assertEqual(4, length(Return)).

test_dead_collector() ->
    Pid = spawn(fun() -> timer:sleep(1) end),
    ?assertEqual({error, unable_to_retrieve_stats},
        perforator_metrics:retrieve(Pid)).
