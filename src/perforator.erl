%% @doc Main API module

-module(perforator).

-export([
    run/1
]).

-define(GENERATOR_FUN_SUFFIX, "_perf_").
-define(TEST_FUN_SUFFIX, "_perf").

-ifdef(TEST).
-compile(export_all).
-endif.


-spec run(Module::atom()) -> ok.
run(Module) ->
    Tests = module_tests(Module),
    run_tests(Tests).

run_tests(_Tests) ->
    throw(not_yet).

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
    NameStr = atom_to_list(FunName),
    (lists:suffix(?GENERATOR_FUN_SUFFIX, NameStr) or
        lists:suffix(?TEST_FUN_SUFFIX, NameStr));
is_test_fun(_) ->
    false.
