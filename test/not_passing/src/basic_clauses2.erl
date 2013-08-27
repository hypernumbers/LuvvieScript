-module(basic_clauses2).

-compile(export_all).

clauses_1() ->
    case_clause(1).

clauses_2()->
   case_clause(2).

clauses_3() ->
    if_clause(44).

clauses_4() ->
    if_clause(3).

clauses_5() ->
    if_clause(2).

case_clause(B) ->
    case B of
        1 -> pandy;
        2 -> andy
    end.

if_clause(B) ->
    if
        B >  3 -> erk;
        B == 3 -> smerk;
        B =< 3 -> berk
    end.



