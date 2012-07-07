-module(perforator_stats_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% Unit Test descriptions
%% ============================================================================

perforator_stats_test_() ->
    {foreach,
        fun() -> ok end,
        fun(_) -> ok end,
        [
            {"Average value calculation test", fun test_average/0},
            {"Min value calculation test", fun test_min/0},
            {"Max value calculation test", fun test_max/0}
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
    ?assertEqual(ExpectedAverage, perforator_stats:average(Reads)).

test_min() ->
    Reads = [
        [{a, 1}, {b, 5}], [{a, 3}, {b, 3}]
    ],
    ExpectedMin = [{a, 1}, {b, 3}],
    ?assertEqual(ExpectedMin, perforator_stats:min(Reads)).

test_max() ->
    Reads = [
        [{a, 1}, {b, 5}], [{a, 3}, {b, 3}]
    ],
    ExpectedMax = [{a, 3}, {b, 5}],
    ?assertEqual(ExpectedMax, perforator_stats:max(Reads)).
