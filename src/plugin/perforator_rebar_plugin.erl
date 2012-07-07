-module(perforator_rebar_plugin).

-compile(export_all).

-define(PERFORATOR_DIR, ".perf").

perf(Config, _AppFile) ->
    ok = filelib:ensure_dir(perforator_dir() ++ "/foobar"),
    ok = filelib:ensure_dir(ebin_dir() ++ "/foobar"),

    CodePath = code:get_path(),
    true = code:add_path(perforator_dir()),
    true = code:add_path(ebin_dir()),

    TestErls = rebar_utils:find_files("test", "\\.erl\$"),
    PerfErls = rebar_utils:find_files("test", "\perf.erl\$"),

    % @todo susidet normalius options (is Config ir siaip gal custom kokiu
    rebar_erlc_compiler:doterl_compile(
        Config, filename:join(?PERFORATOR_DIR, "bin"), PerfErls),

    run_perforator(PerfErls),

    true = code:set_path(CodePath),
    ok.

%% rebar clean hook
clean(_Config, _AppFile) ->
    rebar_file_utils:rm_rf(".perf").

%% ============================================================================
%% Helper functions
%% ============================================================================

run_perforator(Files) ->
    erlang:display("FOOBAR").

perforator_dir() ->
    {ok, Dir} = file:get_cwd(),
    filename:join(Dir, ?PERFORATOR_DIR).

ebin_dir() ->
    {ok, Dir} = file:get_cwd(),
    filename:join(Dir, "ebin").
