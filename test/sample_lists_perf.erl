%% @doc Very simple example for testing different list reversing performance

-module(sample_lists_perf).
-compile(export_all).

-define(LIST_LEN, 10000).

list_operations_perf_() ->
    {foreach,
        fun setup/0,
        fun cleanup/1,
        [
            fun ?MODULE:stupid_list_ops/0,
            fun ?MODULE:good_list_ops/0
        ]
    }.

setup() ->
    ok.

cleanup(_) ->
    ok.

good_list_ops() ->
    lists:reverse(lists:seq(1, ?LIST_LEN)).

stupid_list_ops() ->
    bad_reverse(lists:seq(1, ?LIST_LEN)).

bad_reverse([]) ->
    [];
bad_reverse([X|XS]) ->
    bad_reverse(XS) ++ [X].
