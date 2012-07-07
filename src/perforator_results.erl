-module(perforator_results).

-include("include/log_utils.hrl").

-export([
    save/2
]).

save(Module, TestResults) ->
    application:load(perforator),
    {{Year, Month, Day}, {Hour, Min, _}} = calendar:local_time(),
    Timestamp =
        io_lib:format("[~p~p~p-~p:~p]", [Year, Month, Day, Hour, Min]),
    FilePath = get_dir() ++ atom_to_list(Module) ++
        "_suite_results" ++ Timestamp ++ ".perf",
    ok = filelib:ensure_dir(FilePath),
    ?info("Writing perforator results to file ~s.~n", [FilePath]),
    ok = file:write_file(FilePath,
        io_lib:format("~p.~n", [convert_format(TestResults, get_format())])).

convert_format(TestResults, default) ->
    TestResults;
convert_format(_TestResults, UnsupportedFormat) ->
    ?error("Unknown result_format provided in the conifg",
        [{result_format, UnsupportedFormat}]).

get_format() ->
    case application:get_env(perforator, result_format) of
        undefined ->
            default;
        {ok, ResultFormat} ->
            ResultFormat
    end.

get_dir() ->
    case application:get_env(perforator, result_dir) of
        undefined ->
            ".perf/";
        {ok, ResultDir} ->
            ResultDir
    end.
