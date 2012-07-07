-define(info(A, B), io:format("INFO: " ++ A, B)). %% io:format/2 for now, move to lager
    %%later?
-define(warnining(A, B), io:format("WARNING: " ++ A, B)).
-define(error(A, B), io:format("ERROR: " ++ A, B)).
