-module(perforator_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/perforator.hrl").
-include("include/eunit_utils.hrl").
-include("include/log_utils.hrl").

-define(TEST_FUN_SLEEP, 100). % 100ms

-compile(export_all).

perforator_test_() ->
    {foreach,
        fun setup/0,
        fun cleanup/1,
        [
            {"Correct duration", fun test_correct_duration/0},
            {"{foreach, ..} fixture test", fun test_foreach_perf/0}
        ]
    }.

setup() ->
    application:load(sasl),
    application:set_env(sasl, errlog_type, error),
    ?silent(perforator:ensure_deps_started()),
    perforator:ensure_deps_started(),
    ok.

cleanup(_) ->
    ?silent(perforator:stop_deps()),
    ok.

test_correct_duration() ->
    Test = {raw_fun, {?MODULE, named_fun_perf, 0}},
    Result = perforator:run_test(Test),
    ?assertMatch({named_fun_perf, _}, Result),
    lists:foreach(fun (Run) ->
        Duration = get_duration(Run),
        ?assertApprox(?TEST_FUN_SLEEP*1000, Duration)
    end, get_runs(Result)).

test_foreach_perf() ->
    TestObj = {foreach, fun () -> ok end, fun (_) -> ok end, [
        fun () -> ok end, %% test case 1
        fun (_) -> ok end %% test case 2
    ]},
    Results = perforator:run_test(TestObj),
    ?assertEqual(2, length(Results)),
    Result1 = hd(Results),
    Result2 = hd(tl(Results)),
    Runs1 = get_runs(Result1),
    Runs2 = get_runs(Result2),
    ?assertEqual(?DEFAULT_RUN_COUNT, length(Runs1)),
    ?assertEqual(?DEFAULT_RUN_COUNT, length(Runs2)),
    CheckEverythingFun = fun (Runs) ->
        lists:foreach(fun (Run) ->
            {_, Rez} = Run,
            ?assertMatch({success, _}, Rez),
            {success, Stats} = Rez,
            Duration = proplists:get_value(duration, Stats),
            ?assert(Duration =< 20), %% otherwise something is really baaad
            ?assert(proplists:is_defined(metrics, Stats))
        end, Runs)
    end,
    CheckEverythingFun(Runs1),
    CheckEverythingFun(Runs2).

%% ============================================================================
%% Perf test functions
%% ============================================================================

named_fun_perf() ->
    timer:sleep(?TEST_FUN_SLEEP).


%% ============================================================================
%% Helpers
%% ============================================================================

get_runs({_Name, Results}) ->
    proplists:get_value(runs, Results).

get_duration({_Name, Results}) ->
    proplists:get_value(duration, Results).
