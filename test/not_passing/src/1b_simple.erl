-module('1b_simple').

-export([
         singlefn/1,
         doublefn/2,
         complexfn/1,
         complexfn2/1
        ]).

singlefn(A) ->
    A.

doublefn(A, B) ->
    {A, B}.


complexfn({_A, _B, _C}) ->
    erk.

complexfn2([_A, _B | _C]) ->
    berk.
