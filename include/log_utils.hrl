-define(info(A, B), io:format("INFO: " ++ A, B)). %% io:format/2 for now, move to lager
    %%later?
-define(warnining(A, B), io:format("WARNING: " ++ A, B)).
-define(error(A, B), io:format("ERROR: " ++ A, B)).
-define(silent(Level, Expr), (
        fun() ->
                error_logger:tty(false),
                try
                    % Without this sleep logger will sleep "too early"
                    % (print messages that should not be produced)
                    timer:sleep(1),
                    Expr
                after
                    error_logger:tty(true)
                end
        end)()).
