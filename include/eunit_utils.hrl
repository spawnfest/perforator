%% ============================================================================
%% ?assertSublist(Sublist, List)
%%
%% @doc Checks whether Sublist is a sublist of List, order does not matter.  Can
%% also be used to check whether a proplist is a sublist. Usage of wild cards
%% in {Key, Value} tuples allowed, i.e.  ?assertSublist([{k, '_'}], [{k, 2}])
%% will return 'ok'. Repeated elements matter (we are not checking sets).
%% ============================================================================

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

%% ============================================================================
%% ?assertApprox(ExpectedValue, ObservedValue)
%%
%% @doc Tests for no statistically significant difference between the numeric
%% values.
%% ============================================================================

-define(CONFIDENCE_VALUE, 0.98).

-ifdef(NOASSERT).
-define(assertApprox(ExpectedValue, ObservedValue), ok).
-else.
-define(assertApprox(ExpectedValue, ObservedValue),
    ((fun () ->
        %% wrap values into ( ) because we are in a macro.
        case abs((ObservedValue) - (ExpectedValue)) / (ExpectedValue) of
            X when X =< (1 - (?CONFIDENCE_VALUE)) ->
                ok;
            E ->
                erlang:error({assertApprox_failed, [
                    {module, ?MODULE},
                    {line, ?LINE},
                    {expected_value, ExpectedValue},
                    {observed_value, ObservedValue},
                    {confidence, E}
                ]})
        end
    end)())
).
-endif.
-define(_assertApprox(ExpectedValue, ObservedValue),
    ?_test(?assertSublist(ExpectedValue, ObservedValue))).

