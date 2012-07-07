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
            {"Average value calculation test", fun test_average/0}
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
