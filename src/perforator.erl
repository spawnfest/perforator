%% @doc Main API module
%%
%% @author Ignas Vyšniauskas <i.vysniauskas@gmail.com>

-module(perforator).

-export([
    run/1
]).

-include("include/log_utils.hrl").
-include("include/perforator.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.


-spec run(Module::atom()) -> ok.
run(Module) ->
    ok = ensure_deps_started(),
    Tests = perforator_module_parser:extract_tests(Module),
    TestResults = run_tests(Tests),
    perforator_results:save(Module, TestResults).

run_tests(Tests) ->
    lists:flatten(lists:map(fun (Test) ->
        ?info("Running test ~p~n", [Test]),
        _Results = run_test(Test)
    end, Tests)).

run_test({foreach, SetupFun, CleanupFun, TestObjs}) ->
    lists:map(fun (TestObj) ->
        be_careful(),
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
            run_test(TestObj)
    end;
run_test(PrimitiveTestObj) ->
    case test_obj_is_primitive(PrimitiveTestObj) of
        true ->
            exec_primitive_test_obj(PrimitiveTestObj);
        else ->
            ?error("Unrecognized test object ~p, aborting~n",
                [PrimitiveTestObj]),
            {error, {unknown_test_object, PrimitiveTestObj}}
    end.

exec_primitive_test_obj({repeat, TestObj, _, _}) ->
    %% TBD
   exec_primitive_test_obj(TestObj);
exec_primitive_test_obj({desc, _, TestObj}) ->
    %% TBD
    exec_primitive_test_obj(TestObj);
exec_primitive_test_obj(Fun) ->
    exec_primitive_test_obj(Fun, []).

exec_primitive_test_obj(Fun, Opts) when is_function(Fun) ->
    %% we transform Fun into gay {raw_fun, ...} tuple because
    %% R14B doesn't support  constructing funs from arguments.
    FunInfo = erlang:fun_info(Fun),
    Module = proplists:get_value(module, FunInfo),
    Function = proplists:get_value(function, FunInfo),
    Arity = proplists:get_value(arity, FunInfo),
    RawFun = {raw_fun, {Module, Function, Arity}},
    exec_primitive_test_obj(RawFun, Opts);

exec_primitive_test_obj({raw_fun, {Module, Function, Arity}}, Opts) ->
    RunCount = proplists:get_value(run_count, Opts, ?DEFAULT_RUN_COUNT),
    SleepTime = proplists:get_value(sleep_time, Opts, ?DEFAULT_SLEEP_TIME),
    RunResults = lists:map(fun (RunNum) ->
        try run_testcase_setup(Opts) of
           Args ->
               FunArgs = maybe_strip_args(Arity, Args),
               timer:sleep(SleepTime), %% @todo Make this precise
               Results = {RunNum, perform_run({Module, Function, FunArgs})},
               try run_testcase_cleanup(Opts, Args)
               catch C:R ->
                   ?error("Context cleanup failed: {~p, ~p}~n", [C, R])
               end,
               timer:sleep(SleepTime), %% @todo Make this precise
               Results
        catch
            C:R ->
                ?error("Context setup failed: {~p, ~p}~n", [C, R]),
                {failure, {C, R}}
        end
    end, lists:seq(1, RunCount)),
    {Function, [{runs, RunResults}]}.


run_testcase_setup(Opts) ->
    (proplists:get_value(setup_fun, Opts, fun () -> ok end))().

run_testcase_cleanup(Opts, Args) ->
    (proplists:get_value(cleanup_fun, Opts, fun (_) -> ok end))(Args).

maybe_strip_args(0, _Args) -> []; %% got some args but we don't want them.
maybe_strip_args(_, Args) -> Args.

perform_run({M, F, A}) ->
    Pid = perforator_metrics:init_collect(),
    try timer:tc(M, F, A) of
        {Time, _Value} ->
            {ok, SysMetrics} = perforator_metrics:retrieve(Pid),
            {success, [{duration, Time}, {metrics, SysMetrics}]}
    catch
        C:R ->
            {failure, {C, R}}
    end.


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

ensure_deps_started() ->
    Deps = [sasl, os_mon],
    lists:foreach(fun start_dep/1, Deps).

start_dep(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        _ ->
            throw({unable_to_start_dep, App})
    end.
