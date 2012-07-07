-module(perforator_results).

-include("include/log_utils.hrl").

-export([
    save/2
]).

-define(RESULT_DIR, ".perf/").

save(Module, TestResults) ->
    application:load(perforator),
    case application:get_env(perforator, result_format) of
        undefined ->
            ResultFormat = default;
        ResultFormat ->
            ok
    end,
    FilePath = ?RESULT_DIR ++ atom_to_list(Module) ++ ".perf",
    ok = filelib:ensure_dir(FilePath),
    ?info("Writing perforator results to file ~p.~n", [FilePath]),
    ok = file:write_file(FilePath,
        io_lib:format("~p.", [convert_format(TestResults, ResultFormat)])).

convert_format(TestResults, default) ->
    TestResults;
convert_format(_TestResults, UnsupportedFormat) ->
    ?error("Unknown result_format provided in the conifg",
        [{result_format, UnsupportedFormat}]).
