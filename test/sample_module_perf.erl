%% @doc Sample module used for unit testing.

-module(sample_module_perf).

-compile(export_all).

%% should not be executed during tests
not_a_test_fun() ->
    throw(i_am_evil).

simple_test_perf() ->
    ok.

sleeping_test_perf() ->
    timer:sleep(2000).

generator_test_perf_() ->
    {foreach, fun () -> ok end, fun (_) -> ok end, [
        fun () -> timer:sleep(1000) end
    ]}.
