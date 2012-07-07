%% @doc Perforator types
-module(perforator_types).
-export_type([
    test_fun_desc/0,
    test_fun_type/0,
    test_fun/0
]).


-type test_fun_desc() :: {test_fun_type(), test_fun()}.
-type test_fun_type() :: raw | generator.
-type test_fun() :: fun(() -> term()).
