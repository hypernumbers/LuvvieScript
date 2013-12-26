-module('1f_multiple_arity_fns_with_exports').

-export([
         arity/0
        ]).

arity() ->
    arity(erk).


arity(B) ->
    B.
