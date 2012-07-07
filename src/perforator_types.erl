%% @doc perforator types
%%
%% @author Ignas Vyšniauskas <i.vysniauskas@gmail.com>

-module(perforator_types).

-export_type([
    test_fun/0,
    test_obj/0,
    primitive_test_obj/0,
    fixture_test_obj/0,
    run_results/0,
    test_case_results/0,
    test_case_opts/0,
    fun_spec/0
]).

-type test_obj() :: primitive_test_obj() | fixture_test_obj().
-type fixture_test_obj() :: setup_fixture() | foreach_fixture().
-type setup_fixture() ::
    {setup, Setup :: fun(() -> any()), Cleanup :: fun((any()) -> any()),
        test_obj()}.
-type foreach_fixture() ::
    {foreach, Setup :: fun(() -> any()), Cleanup :: fun((any()) -> any()),
        [test_obj()]}.
%% @todo Fix!
-type primitive_test_obj() :: test_fun() | test_fun_decorator() | fun().
-type test_fun_decorator() :: repeat_decorator() | desc_decorator().
-type repeat_decorator() ::
    {repeat, test_fun(), Times :: pos_integer(), Sleep :: timer:time()}.
-type desc_decorator() ::
    {desc, string(), test_fun()}.
-type test_fun() :: {raw_fun, mfa_spec()}.

-type fun_spec() :: mfa_spec() | fun().
-type mfa_spec() :: {Module::atom(), Funtion::atom(), Arity::non_neg_integer()}.

-type run_results() :: term(). %% @todo
-type test_case_results() :: term(). %% @todo
-type test_case_opts() :: term(). %% @todo
