%% @doc Parses _perf.erl modules, extracts funs and fixtures via module_info().
%%
%% @author Ignas Vyšniauskas <i.vysniauskas@gmail.com>

-module(perforator_module_parser).

-include("include/perforator.hrl").

-export([
    extract_tests/1
]).

-spec extract_tests(atom()) -> perforator_types:test_obj().
extract_tests(Module) ->
    try Module:module_info(exports) of
        Exports ->
            get_test_objects(Module, Exports)
    catch
        error:undef ->
            throw(module_not_found)
    end.
get_test_objects(Module, Exports) ->
    get_test_objects(Module, Exports, []).

get_test_objects(_Module, [], Acc) ->
    Acc;
get_test_objects(Module, [{FunName, 0}|Rest], Acc) ->
    %% @todo rewrite this, ewww
    case is_raw_test_fun(FunName) of
        true ->
            TestObj = {raw_fun, {Module, FunName, 0}},
            get_test_objects(Module, Rest, [TestObj|Acc]);
        false ->
            case is_generator_fun(FunName) of
                true ->
                    TestObj = apply(Module, FunName, []),
                    get_test_objects(Module, Rest, [TestObj|Acc]);
                false ->
                    get_test_objects(Module, Rest, Acc)
            end
    end;
get_test_objects(Module, [_|Rest], Acc) ->
    get_test_objects(Module, Rest, Acc).

is_generator_fun(FunName) ->
    NameStr = atom_to_list(FunName),
    lists:suffix(?GENERATOR_FUN_SUFFIX, NameStr).

is_raw_test_fun(FunName) ->
    NameStr = atom_to_list(FunName),
    lists:suffix(?TEST_FUN_SUFFIX, NameStr).

