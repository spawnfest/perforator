-module(perforator_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/perforator.hrl").
-include("include/eunit_utils.hrl").
-include("include/log_utils.hrl").

-compile(export_all).

perforator_test_() ->
    {foreach,
        fun setup/0,
        fun cleanup/1,
        [
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

test_foreach_perf() ->
    Test = {foreach, fun () -> ok end, fun (_) -> ok end, [
        fun () -> ok end,
        fun (_) -> ok end
    ]},
    Results = perforator:run_test(Test),
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
%% Helpers
%% ============================================================================

get_runs({_Name, Results}) ->
    proplists:get_value(runs, Results, []).

