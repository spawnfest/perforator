%% ============================================================================
%% Log utils
%% ============================================================================

% Use these only for pretty prints in tests/console!
-define(DEFAULT_INFO(Msg), [
    {pid, self()},
    {source, ?FILE ++ ":" ++ integer_to_list(?LINE)},
    {message, Msg}
]).

-define(error(Msg, Opts),
    error_logger:error_report(?DEFAULT_INFO(Msg) ++ Opts)).

-define(status(Msg, Opts),
    io:format(Msg, Opts)).

-define(silent(Expr), (
        fun() ->
                error_logger:tty(false),
                try
                    Expr
                catch
                    C:R ->
                        error_logger:tty(true),
                        ?error("shouldn't have silenced here", [{C, R}])
                after
                    error_logger:tty(true)
                end
        end)()).
