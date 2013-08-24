-module(basic_ifmod).

-compile(export_all).

if_fn() ->
    N = 5,
    if
        N > 5 ->
            ok;
        N =< 5 ->
            dont
    end.
