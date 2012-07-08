%% @doc Main API module
%%
%% @author Ignas Vyšniauskas <i.vysniauskas@gmail.com>

-module(perforator).

-export([
    run/1
]).

-include("log_utils.hrl").
-include("perforator.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-spec run(Module::atom()) -> ok.
run(Module) ->
    ?silent(ok = ensure_deps_started()),
    Tests = perforator_module_parser:extract_test_objs(Module),
    ?status("====================================================~n", []),
    ?status("Perforating module: ~p~n", [Module]),
    ?status("====================================================~n", []),
    TestResults = run_tests(Tests),
    ok = perforator_results:save(Module, TestResults),
    ?silent(stop_deps()).

run_tests(Tests) ->
    lists:flatten(lists:map(fun (Test) ->
        _Results = run_test(Test)
    end, Tests)).

run_test({foreach, SetupFun, CleanupFun, TestObjs}) ->
    lists:map(fun (TestObj) ->
        run_test({setup, SetupFun, CleanupFun, TestObj})
    end, TestObjs);
run_test({setup, SetupFun, CleanupFun, TestObj}) ->
    case test_obj_is_primitive(TestObj) of
        true ->
            exec_primitive_test_obj(TestObj, [
                {setup_fun, SetupFun},
                {cleanup_fun, CleanupFun}
            ]);
        false ->
            %% @todo: refactor!
            try run_testcase_setup([]) of
               Args ->
                   be_careful(), %% @todo Make this precise
                   Results = run_test(TestObj),
                   try run_testcase_cleanup([], Args)
                   catch C:R ->
                       ?error("Context cleanup failed", [{C, R}])
                   end,
                   be_careful(), %% @todo Make this precise
                   Results
            catch
                C:R ->
                    ?error("Context setup failed", [{C, R}]),
                    {failure, {C, R}}
            end
    end;
run_test(PrimitiveTestObj) ->
    case test_obj_is_primitive(PrimitiveTestObj) of
        true ->
            exec_primitive_test_obj(PrimitiveTestObj);
        false ->
            ?error("Unrecognized test object, aborting",
                [{obj, PrimitiveTestObj}]),
            {error, {unknown_test_object, PrimitiveTestObj}}
    end.

exec_primitive_test_obj({repeat, TestObj, _, _}) ->
    %% @todo: TBD
   exec_primitive_test_obj(TestObj);
exec_primitive_test_obj({desc, _, TestObj}) ->
    %% @todo: TBD
    exec_primitive_test_obj(TestObj);
exec_primitive_test_obj(Fun) ->
    exec_primitive_test_obj(Fun, []).

exec_primitive_test_obj(Fun, Opts) when is_function(Fun) ->
    exec_test_case(Fun, Opts);

exec_primitive_test_obj({raw_fun, FunSpec={_Module, _Function, _Arity}}, Opts) ->
    exec_test_case(FunSpec, Opts).

-spec exec_test_case(perforator_types:fun_spec(),
    perforator_types:test_case_opts()) -> perforator_types:test_case_results().
exec_test_case(FunSpec, Opts) ->
    RunCount = proplists:get_value(run_count, Opts, ?DEFAULT_RUN_COUNT),
    SleepTime = proplists:get_value(sleep_time, Opts, ?DEFAULT_SLEEP_TIME),
    ?status("Running test: ~p...~n", [get_test_case_name(FunSpec)]),
    RunResults = lists:map(fun (RunNum) ->
        try run_testcase_setup(Opts) of
           Args ->
               timer:sleep(SleepTime), %% @todo Make this precise
               Results = {RunNum, perform_run(FunSpec, Args)},
               try run_testcase_cleanup(Opts, Args)
               catch C:R ->
                   ?error("Context cleanup failed", [{C, R}])
               end,
               timer:sleep(SleepTime), %% @todo Make this precise
               Results
        catch
            C:R ->
                ?error("Context setup failed:", [{C, R}]),
                {failure, {C, R}}
        end
    end, lists:seq(1, RunCount)),
    TestCaseName = get_test_case_name(FunSpec),
    {TestCaseName, [{runs, RunResults}]}.

%% @doc Sorry for this FunSpec crap, but this is needed to:
%% 1) stay comptabile with pre-R15B Erlang, because I cannot do fun
%% Module:Function/Arity.
%% 2) Not wrap stuff into fun's (to influence performance less)
-spec perform_run(perforator_types:fun_spec(), [term()]) ->
    perforator_types:run_results().
perform_run(FunSpec, Args) ->
    Arity = case FunSpec of
        Fun when is_function(Fun) ->
            proplists:get_value(arity, erlang:fun_info(Fun));
        {_M, _F, Ar} ->
            Ar
    end,
    FunArgs = maybe_strip_args(Arity, Args),
    Pid = perforator_metrics:init_collect(),
    try
        case FunSpec of
            FunA when is_function(FunA) -> timer:tc(FunA, FunArgs);
            {M, F, _} -> timer:tc(M, F, FunArgs)
        end
    of
        {Time, _Value} ->
            {ok, SysMetrics} = perforator_metrics:retrieve(Pid),
            {success, [{duration, Time}, {metrics, SysMetrics}]}
    catch
        C:R ->
            {failure, {C, R}}
    end.

-spec get_test_case_name(perforator_types:fun_spec()) -> atom().
get_test_case_name(Fun) when is_function(Fun) ->
    proplists:get_value(name, erlang:fun_info(Fun));
get_test_case_name({_M, F, _Ar}) ->
    F.

run_testcase_setup(Opts) ->
    (proplists:get_value(setup_fun, Opts, fun () -> ok end))().

run_testcase_cleanup(Opts, Args) ->
    (proplists:get_value(cleanup_fun, Opts, fun (_) -> ok end))(Args).

maybe_strip_args(0, _Args) -> []; %% got some args but we don't want them.
maybe_strip_args(_Arity, Args) -> [Args].

%% ============================================================================
%% Type checks
%% ============================================================================

test_obj_is_primitive(TestObj) ->
    sheriff:check(TestObj, {perforator_types, primitive_test_obj}).

%% ============================================================================
%% Throwaway helper functions
%% ============================================================================

be_careful() ->
    erlang:garbage_collect(),
    timer:sleep(500).

deps() -> [sasl, os_mon].

ensure_deps_started() ->
    application:load(sasl),
    application:set_env(sasl, errlog_type, error),
    lists:foreach(fun start_dep/1, deps()).

stop_deps() ->
    ?silent(lists:foreach(fun application:stop/1, lists:reverse(deps()))).

start_dep(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        _ ->
            throw({unable_to_start_dep, App})
    end.
