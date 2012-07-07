%% @doc Main API module

-module(perforator).

-export([
    run/1
]).

-include("log_utils.hrl").

-define(TEST_FUN_SUFFIX, "_perf").
-define(GENERATOR_FUN_SUFFIX, "_perf_").
-define(RESULT_DIR, ".perf/").

-ifdef(TEST).
-compile(export_all).
-endif.


-spec run(Module::atom()) -> ok.
run(Module) ->
    ok = ensure_deps_started(),
    _RunDate = calendar:local_time(),
    Tests = module_tests(Module),
    TestResults = run_tests(Tests),
    save_results(Module, TestResults).

run_tests(Tests) ->
    lists:map(fun (Test) ->
        case run_test(Test) of
            {ok, Results} ->
                {Test, Results};
            {error, Error} ->
                {Test, {failure, Error}}
        end
    end, Tests).

%% simple _perf() fun.
-spec run_test(TestFun :: fun(() -> term())) ->
    {ok, Results :: list()} | {error, Error :: list()}.
run_test(TestFun) ->
    Pid = perforator_metrics:init_collect(),
    try timer:tc(TestFun) of
        {Time, _Value} ->
            {ok, SysMetrics} = perforator_metrics:retrieve(Pid),
            {ok, [{duration, Time}|SysMetrics]}
    catch
        C:R ->
            {error, {C, R}}
    end.

save_results(Module, TestResults) ->
    FilePath = ?RESULT_DIR ++ atom_to_list(Module) ++ ".perf",
    ok = filelib:ensure_dir(FilePath),
    ?info("Writing perforator results to file ~p.~n", [FilePath]),
    ok = file:write_file(FilePath, io_lib:format("~p", [TestResults])).

module_tests(Module) ->
    try Module:module_info(exports) of
        Exps ->
            FunArrList = filter_test_funs(Exps),
            [FunName || {FunName, _} <- FunArrList]
    catch
        error:undef ->
            throw(module_not_found)
    end.

filter_test_funs(Exports) ->
    lists:filter(fun is_test_fun/1, Exports).

is_test_fun({FunName, 0}) when is_atom(FunName) ->
    is_generator_fun(FunName) or is_simple_test_fun(FunName);
is_test_fun(_) ->
    false.

is_generator_fun(FunName) ->
    NameStr = atom_to_list(FunName),
    lists:suffix(?GENERATOR_FUN_SUFFIX, NameStr).

is_simple_test_fun(FunName) ->
    NameStr = atom_to_list(FunName),
    lists:suffix(?TEST_FUN_SUFFIX, NameStr).

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
