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
    perforator_results:save(Module, TestResults).

run_tests(Tests) ->
    lists:map(fun (Test) ->
        case run_test(Test) of
            {ok, Results} ->
                {Test, Results};
            {error, Error} ->
                {Test, {failure, Error}}
        end
    end, Tests).

exec_fun(TestFun) ->
    Pid = perforator_metrics:init_collect(),
    try timer:tc(TestFun) of
        {Time, _Value} ->
            {ok, SysMetrics} = perforator_metrics:retrieve(Pid),
            {ok, [{duration, Time}|SysMetrics]}
    catch
        C:R ->
            {error, {C, R}}
    end.

%% simple _perf() fun.
-spec run_test(perforator_types:test_fun_desc()) ->
    {ok, Results :: list()} | {error, Error :: list()}.
run_test({raw, TestFun}) ->
    exec_fun(TestFun);

run_test({generator, _TestFun}) ->
    {error, not_implemented}.

module_tests(Module) ->
    try Module:module_info(exports) of
        Exps ->
            filter_and_tag_test_funs(Module, Exps)
    catch
        error:undef ->
            throw(module_not_found)
    end.

filter_and_tag_test_funs(Module, Exports) ->
    filter_and_tag_test_funs(Module, Exports, []).

filter_and_tag_test_funs(_Module, [], Acc) ->
    Acc;
filter_and_tag_test_funs(Module, [{FunName, 0}|Rest], Acc) ->
    %% @todo rewrite this, ewww
    case is_simple_test_fun(FunName) of
        true ->
            TaggedFun = {raw, fun() ->  Module:FunName() end},
            filter_and_tag_test_funs(Module, Rest, [TaggedFun|Acc]);
        false ->
            case is_generator_fun(FunName) of
                true ->
                    TaggedFun = {generator, fun() ->  Module:FunName() end},
                    filter_and_tag_test_funs(Module, Rest, [TaggedFun|Acc]);
                false ->
                    filter_and_tag_test_funs(Module, Rest, Acc)
            end
    end;
filter_and_tag_test_funs(Module, [_|Rest], Acc) ->
    filter_and_tag_test_funs(Module, Rest, Acc).

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
