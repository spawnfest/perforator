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
    ?status("Writing perforator results to file ~s~n", [FilePath]),
    ok = file:write_file(FilePath,
        io_lib:format("~p.~n",
            [convert_format(Module, TestResults, get_format())]
        )
    ).

convert_format(Module, TestResults, default) ->
    TestsOutput = default_results_parse(TestResults),
    {list_to_binary(atom_to_list(Module)), [
        default_header(),
        default_totals(TestsOutput),
        TestsOutput
    ]};

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
    {test_cases, [
            calc_test_case_output(TestCase) ||
            TestCase <- TestResults
    ]}.

default_header() ->
    {Mega, Secs, _} = now(),
    Timestamp = Mega*1000000 + Secs,
    {date, Timestamp}.

default_totals({test_cases, TestsOutput}) ->
    {totals, [
        {test_count, length(TestsOutput)},
        {failure_count, get_total_failures(TestsOutput)},
        {duration, get_total_duration(TestsOutput)}
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
    };
get_metrics({RunId, {failure, _Info}}) ->
    {RunId,
        [
            {success, false}
        ]
    }.

%% @doc wrongly assumes that all tests have succeeded
get_total_duration(TestsOutput) ->
    lists:foldl(
        fun(TestCase, Acc) ->
            {_CaseName, CaseData} = TestCase,
            case proplists:get_value(successful, CaseData) of
                true ->
                    RunCount = proplists:get_value(run_count,
                        proplists:get_value(test_conditions, CaseData)),
                    AvgDuration =
                        proplists:get_value(mean,
                            proplists:get_value(duration,
                                proplists:get_value(result, CaseData)
                            )
                        ),
                    RunCount * AvgDuration + Acc;
                false ->
                    Acc
            end
        end,
        0,
        TestsOutput
    ).

get_total_failures(TestsOutput) ->
    lists:foldl(
        fun(TestCase, Acc) ->
            {_CaseName, CaseData} = TestCase,
            case proplists:get_value(successful, CaseData) of
                true ->
                    CaseFails = proplists:get_value(
                        failures,
                        proplists:get_value(result, CaseData)),
                    CaseFails + Acc;
                false ->
                    % @todo get this from somewhere
                    ?DEFAULT_RUN_COUNT + Acc
            end
        end,
        0,
        TestsOutput
    ).

calc_test_case_averages(TestCaseMetrics) ->
    % @todo use mean instead of average all the way down (and up)
    {mean, perforator_stats:average([
        [proplists:lookup(duration, Metric)]
            ++ proplists:get_value(mean, Metric) ||
        Metric <- TestCaseMetrics
        ]
    )}.

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
    case get_runs(TestCase) of
        [{failure, _} | _] ->
            format_failing_setup(TestCase);
        _ ->
            Runs = [get_metrics(Run) ||
                Run={_Id, {success, _}} <- get_runs(TestCase)],
            RunsResults = [RunResult ||
                {_Id, [{success, true}, {results, RunResult}]} <- Runs],
            RunsFailures = length([ 1 ||
                {_Id, [{success, false}]} <- Runs]),
            case RunsResults of
                [] -> format_failing_case_output(CaseName, TestCase);
                _ ->
                    format_successful_case_output(CaseName,
                        Runs, RunsResults, RunsFailures)
            end
    end.

format_failing_setup(TestCase={CaseName, _}) ->
    [{failure, Info} | _ ] = get_runs(TestCase),
    {list_to_binary(atom_to_list(CaseName)), [
        {successful, false},
        {failure, Info}
    ]}.

format_failing_case_output(CaseName, TestCase) ->
    Runs = get_runs(TestCase),
    [{_Id, {failure, Info}} | _ ] = Runs,
    {list_to_binary(atom_to_list(CaseName)), [
        {successful, false},
        {failure, Info}
    ]}.

format_successful_case_output(CaseName, Runs, RunsResults, RunsFailures) ->
    {list_to_binary(atom_to_list(CaseName)), [
            {successful, true},
            {test_conditions, [
                {run_count, length(RunsResults)},
                {sleep_time, get_sleep_time()}
            ]},
            {result,
                [{failures, RunsFailures}] ++
                reformat_ble([
                    calc_test_case_averages(RunsResults),
                    calc_test_case_mins(RunsResults),
                    calc_test_case_maxs(RunsResults)
                ])
            },
            {runs,
                [{Id, [{results, Results}]} ||
                    {Id, [{success, true}, {results, Results}]}<-Runs
                ]
            }
    ]}.

get_sleep_time() ->
    ?DEFAULT_SLEEP_TIME.

%% @todo FIX WAT IDENTATION
reformat_ble(Proplists = [{_, Stats} | _ ]) ->
   [{Metric,
        [{Tag, proplists:get_value(Metric, List)} || {Tag, List} <- Proplists]}
        || {Metric, _Val} <- Stats].
