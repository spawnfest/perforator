-module(perforator_results_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(RESULT_DIR, ".perf/").

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
    perforator_results:save(sample_module_perf, [perforator_utils:get_timestamp(),
        [{snacks_consumed, 9001}]]),
    FilePath = ?RESULT_DIR ++ atom_to_list(sample_module_perf) ++ ".perf",
    {ok, [[_Timestamp, [Contents]]]} = file:consult(FilePath),
    ?assertEqual({snacks_consumed, 9001}, Contents).
