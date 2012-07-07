-module(perforator_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_utils.hrl").
-include("include/log_utils.hrl").

-compile(export_all).

perforator_test_() ->
    {foreach,
        fun setup/0,
        fun cleanup/1,
        [
            {"{foreach, ..} fixture test",
                {timeout, 10000, fun test_foreach_perf/0}}
        ]
    }.

setup() ->
    application:load(sasl),
    ok = perforator:ensure_deps_started(),
    timer:sleep(100),
    ok.

cleanup(_) ->
    ok.

test_foreach_perf() ->
    Test = {foreach, fun () -> ok end, fun (_) -> ok end, [
        fun () -> timer:sleep(1000) end,
        fun () -> timer:sleep(1000) end,
        fun (_) -> timer:sleep(1000) end
    ]},
    Results = perforator:run_test(Test),
    ?assert(false).
