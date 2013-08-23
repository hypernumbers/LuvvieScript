-module(basic_clauses2).

-compile(export_all).

clauses() ->
    B = 1,
    case B of
        1 ->
            if
                B > 2 ->
                    erk;
                true ->
                    berk
            end;
        2 -> andy
    end.


