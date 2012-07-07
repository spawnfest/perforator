-module(perforator_results_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% Unit Test descriptions
%% ============================================================================

perforator_results_test_() ->
    {foreach,
        fun() -> ok end,
        fun(_) -> ok end,
        [
            {"Save results test", fun test_save_results/0}
        ]
    }.

%% ============================================================================
%% Actual Tests
%% ============================================================================

test_save_results() ->
    application:load(perforator),
    application:set_env(perforator, result_dir, ".perf_test/"),
    perforator_results:save(sample_module_perf, {perforator_utils:get_timestamp(),
        [{snacks_consumed, 9001}]}),
    FilePath = ".perf_test/" ++ atom_to_list(sample_module_perf) ++ ".perf",
    {ok, [{_Timestamp, [Contents]}]} = file:consult(FilePath),
    ?assertEqual({snacks_consumed, 9001}, Contents).
