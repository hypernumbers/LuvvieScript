-module('1e_multiple_arity_fns').

-export([
         arity/0,
         arity/1,
         inserted/0
        ]).

arity() ->
    erk.

%% inserted just mucks up the order inside the AST
inserted() ->
    dynamite.

arity(B) ->
    B.
