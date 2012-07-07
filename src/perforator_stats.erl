%% @doc Various statistics functions
-module(perforator_stats).

-export([
    mean/1
]).

mean([]) ->
    0;
mean(List) ->
    lists:sum(List) / lists:length(List).
