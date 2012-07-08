-module(perforator_results).

-include("include/log_utils.hrl").
-include("include/perforator.hrl").

-export([
    save/2
]).

save(Module, TestResults) ->
    application:load(perforator),
    {{Year, Month, Day}, {Hour, Min, _}} = calendar:local_time(),
    Timestamp = io_lib:format(
        "~4.10.0B~2.10.0B~2.10.0BT~2.10.0B~2.10.0B",
        [Year, Month, Day, Hour, Min]
    ),
    FilePath = get_dir() ++ atom_to_list(Module) ++
        "_suite_results_" ++ Timestamp ++ ".perf",
    ok = filelib:ensure_dir(FilePath),
    ?info("Writing perforator results to file ~s.~n", [FilePath]),
    ok = file:write_file(FilePath,
        io_lib:format("~p.~n",
            [convert_format(Module, TestResults, get_format())]
        )
    ).

convert_format(Module, TestResults, default) ->
    TestsOutput = default_results_parse(TestResults),
    [
        default_header(Module),
        default_totals(TestsOutput),
        TestsOutput
    ];

convert_format(_Module, _TestResults, UnsupportedFormat) ->
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

default_results_parse(TestResults) ->
    {tests, [
            calc_test_case_output(TestCase) ||
            TestCase <- TestResults
    ]}.

default_header(Module) ->
    {{Year, Month, Day}, {Hour, Min, _}} = calendar:local_time(),
    Timestamp = io_lib:format(
        "~4.10.0B~2.10.0B~2.10.0BT~2.10.0B~2.10.0B",
        [Year, Month, Day, Hour, Min]
    ),
    [
        {test_suite, atom_to_list(Module)},
        {datetime, Timestamp}
    ].

default_totals({tests, TestsOutput}) ->
    {totals, [
        {test_count, length(TestsOutput)},
        {failure_count, get_failures(TestsOutput)}
    ]}.

-spec get_runs(perforator_results_types:test_case()) ->
    [perforator_results_types:test_case_run()].
get_runs({_CaseName, [{runs, Runs}]}) ->
    Runs.

-spec get_metrics(perforator_results_types:test_case_run()) ->
    [perforator_results_types:test_case_run_tick()].
get_metrics({RunId, {success, RunStats}}) ->
    Duration = proplists:lookup(duration, RunStats),
    TickData = proplists:get_value(metrics, RunStats),
    TickMetrics = [Metrics || {_TimeStamp, Metrics} <-TickData],
    {RunId,
        [
            {success, true},
            {results, [Duration] ++  perforator_metrics:agregate(TickMetrics)}
        ]
    }.

get_failures(TestsOutput) ->
    lists:foldl(
        fun(TestCase, Acc) ->
            {_CaseName, CaseData} = TestCase,
            CaseFails = proplists:get_value(
                failures,
                proplists:get_value(result, CaseData)),
            CaseFails + Acc
        end,
        0,
        TestsOutput
    ).

calc_test_case_averages(TestCaseMetrics) ->
    {average, [{results, perforator_stats:average([
        [proplists:lookup(duration, Metric)]
            ++ proplists:get_value(average, Metric) ||
        Metric <- TestCaseMetrics
        ]
    )}]}.

calc_test_case_mins(TestCaseMetrics) ->
    {min, perforator_stats:min([
        [proplists:lookup(duration, Metric)]
            ++ proplists:get_value(min, Metric) ||
        Metric <- TestCaseMetrics
    ])}.

calc_test_case_maxs(TestCaseMetrics) ->
    {max, perforator_stats:max([
        [proplists:lookup(duration, Metric)]
            ++ proplists:get_value(max, Metric) ||
        Metric <- TestCaseMetrics
        ]
    )}.

calc_test_case_output(TestCase={CaseName, _}) ->
    Runs = [get_metrics(Run) || Run <- get_runs(TestCase)],
    RunsResults = [RunResult ||
        {_Id, [{success, _}, {results, RunResult}]} <- Runs],
    RunsFailures = length([ 1 ||
        {_Id, [{success, false}, {results, _}]} <- Runs]),
    {atom_to_list(CaseName), [
        {test_conditions, [
            {run_count, length(RunsResults)},
            {sleep_time, get_sleep_time()}
        ]},
        {result, [
            {failures, RunsFailures},
            calc_test_case_averages(RunsResults),
            calc_test_case_mins(RunsResults),
            calc_test_case_maxs(RunsResults)
        ]},
        {runs,
            [{Id, [{results, Results}]} ||
                {Id, [{success, _}, {results, Results}]}<-Runs
            ]
        }
    ]}.

get_sleep_time() ->
    ?DEFAULT_SLEEP_TIME.
