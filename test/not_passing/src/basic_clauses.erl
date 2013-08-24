-module(basic_clauses).

-compile(export_all).

-record(myrec, {bish,
                   bosh}).

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
    ok.

clause(X, Y) when is_function(X, Y) ->
    ok.
