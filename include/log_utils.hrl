-define(info(A, B), io:format("INFO: " ++ A, B)). %% io:format/2 for now, move to lager
    %%later?
-define(warnining(A, B), io:format("WARNING: " ++ A, B)).
-define(error(A, B), io:format("ERROR: " ++ A, B)).
-define(silent(Expr), (
        fun() ->
                error_logger:tty(false),
                try
                    Expr
                catch
                    C:R ->
                        error_logger:tty(true),
                        ?error("shouldn't have silenced here", [C,R])
                after
                    error_logger:tty(true)
                end
        end)()).
