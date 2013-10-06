-module(basic_clauses).

-compile(export_all).

-record(myrec, {bish,
                   bosh}).


atom() ->
    clause(berk).

boolean() ->
    clause(true).

float() ->
    clause(1.2).

function1() ->
    Fun = fun() ->
                  ok
          end,
    clause(Fun).

function2() ->
    Fun = fun(_Bish, _Bash, _Bosh) ->
                  ok
          end,
    clause(Fun, 3).

function3() ->
    Fun = fun(_Bish, _Bash, _Bosh) ->
                  ok;
             (erk, Berk, Jerk) ->
                  ok;
             (Bingo, Bango, Bongo) when is_list(Bingo)  andalso
                                        is_tuple(Bango) orelse
                                        is_atom(Bongo)  ->
                  ok
          end,
    clause(Fun, 3).


integer() ->
    clause(1).

number1() ->
    clause(1.3).

number2() ->
    clause(1).

pid() ->
    clause(self()).

record() ->
    clause(#myrec{}).

reference() ->
    clause(make_ref()).

tuple() ->
    clause({1, 2, 3}).

list() ->
    clause([bish, bash, bosh]).

clause(X) when is_atom(X) ->
    ok;
clause(X) when is_boolean(X) ->
    ok;
clause(X) when is_float(X) ->
    ok;
clause(X) when is_function(X) ->
    ok;
clause(X) when is_integer(X) ->
    ok;
clause(X) when is_number(X) ->
    ok;
clause(X) when is_pid(X) ->
    ok;
clause(X) when is_record(X, myrec) ->
    ok;
clause(X) when is_reference(X) ->
    ok;
clause(X) when is_tuple(X) ->
    ok;
clause(X) when is_list(X) ->
    ok.

clause(X, Y) when is_function(X, Y) ->
    ok.

multiguard(X) when is_list(X) orelse
                   is_tuple(X) ->
    ok;
multiguard(X) when is_list(X) andalso
                   is_tuple(X) ->
    ok;
multiguard(X) when is_list(X), is_tuple(X) ->
    ok;
multiguard(X) when is_list(X); is_tuple(X) ->
    ok.
