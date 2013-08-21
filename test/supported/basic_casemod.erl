-module(casemod).

-compile(export_all).

casefn() ->
    N = 5,
    case N of
        5 ->
            ok;
        _ ->
            erk
    end.
