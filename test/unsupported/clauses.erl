-module(clauses).

-compile(export_all).

clause(X) when is_binary(X) ->
    ok;
clause(X) when is_bitstring(X) ->
    ok;
clause(X) when is_port(X) ->
    ok.
