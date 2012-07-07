-module(perforator_metrics_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

average_test() ->
    Reads = [
        [{cpu_util, N}, {cpu_load, N+3}] || N <- lists:seq(1, 3)
    ],
    ExpectedAverage = [{cpu_util, 2.0}, {cpu_load, 5.0}],
    ?assertEqual(ExpectedAverage, perforator_metrics:average(Reads)).
