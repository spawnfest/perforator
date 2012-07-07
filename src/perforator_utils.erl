-module(perforator_utils).

-export([
    get_timestamp/0
]).

-spec get_timestamp() -> integer(). % timestamp in microseconds
get_timestamp() ->
    {Mega, Sec, Micro} = erlang:now(),
    (Mega * 1000000 + Sec) * 1000000 + Micro.
