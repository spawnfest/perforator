%% Checks whether Sublist is a sublist of List, order does not matter.  Can
%% also be used to check whether a proplist is a sublist. Usage of wild cards
%% in {Key, Value} tuples allowed, i.e.  ?assertSublist([{k, '_'}], [{k, 2}])
%% will return 'ok'. Repeated elements matter (we are not checking sets).

-ifdef(NOASSERT).
-define(assertSublist(Sublist, List), ok).
-else.
-define(assertSublist(Sublist, List),
	((fun () ->
        WildcardSublist = [A || {_, '_'}=A <- Sublist],
        PureSublist = Sublist -- WildcardSublist,
        IsSubListFun = fun
            ([], _List, _) ->
                true;
            ([H|T], List, Self) ->
                case lists:member(H, List) of
                    true ->
                        Self(T, lists:delete(H, List), Self);
                    false ->
                        false
                end
        end,

        case IsSubListFun(PureSublist, List, IsSubListFun) of
            true ->
                ok;
            false ->
                erlang:error({assertSublist_failed, [
                    {module, ?MODULE},
                    {line, ?LINE},
                    {sublist, Sublist},
                    {list, List}
                ]})
        end,

        lists:foreach(
            fun ({Key, _}) ->
                case length([H || {K, _}=H <- List -- PureSublist, Key == K]) of
                    0 ->
                        erlang:error({assertSublist_failed, [
                            {module, ?MODULE},
                            {line, ?LINE},
                            {sublist, Sublist},
                            {list, List},
                            {key, Key}
                        ]});
                    _ ->
                        ok
                end
            end,
            WildcardSublist
        )
    end)())
).
-endif.
-define(_assertSublist(Sublist, List), ?_test(?assertSublist(Sublist, List))).

