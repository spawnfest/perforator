-module(perforator_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

module_tests_test() ->
    ?assertEqual(
        lists:sort([simple_test_perf, sleeping_test_perf,
            generator_test_perf_]),
        lists:sort(perforator:module_tests(sample_module_perf))
    ).

