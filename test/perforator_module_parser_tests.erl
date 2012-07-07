-module(perforator_module_parser_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_utils.hrl").

export_tests_test() ->
    TestModule = sample_module_perf,
    Tests = perforator_module_parser:extract_test_objs(TestModule),
    ?assertSublist(
        [{raw_fun, {TestModule, simple_test_perf, 0}},
        {raw_fun, {TestModule, sleeping_test_perf, 0}}],
        Tests
    ),
    ?assertMatch(
        {foreach, _, _, [_]},
        hd([X || X={foreach, _, _, _} <- Tests])
    ).

