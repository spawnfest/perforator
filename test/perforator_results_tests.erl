-module(perforator_results_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("include/log_utils.hrl").

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
    application:set_env(perforator, result_dir, ".perf/"),
    ?silent(perforator_results:save(foobar_perf,
        [{foobar_perf,
            [{runs,
                [{1,
                    {success, [
                        {duration, 110},
                        {metrics, [
                            {42, [
                                {snacks_consumed, 9001}
                            ]}
                        ]}
                    ]}
                }]
            }]
        }])),
    {ok,ListDir} = file:list_dir(".perf"),
    BasenameList = [File ||
        File <- ListDir, lists:prefix(atom_to_list(foobar_perf), File)],
    [LastFile | _] = lists:sort(fun(A, B) -> A > B end, BasenameList),
    FilePath = ".perf/" ++ LastFile,
    {ok, Contents} = file:consult(FilePath),
    ?assertMatch(
        [{_, [
            {date, _},
            {totals, [
                {test_count, _},
                {failure_count, _},
                {duration, _}
            ]},
            {test_cases, [
                {<<"foobar_perf">>, [
                    {successful, true},
                    {test_conditions, _},
                    {result, _},
                    {runs, _}
                ]}
            ]}
        ]}],
        Contents
    ).
