%% @doc Sample module used for unit testing.

-module(failing_module_perf).

-compile(export_all).


simple_fail_perf() ->
    throw(rocks).

setup_fail_perf_() ->
    {setup,
        fun() -> throw(did_not_expect_this_did_you) end,
        fun(_) -> ok end,
        fun() -> timer:sleep(50) end
    }.
