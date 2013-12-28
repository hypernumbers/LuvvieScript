-module('2c_assignment').

-export([
         assignment_fn/0
        ]).

assignment_fn() ->
    A = 1,
    B = 2,
    C = 0,
    {A, B} = {1 + C, 2 + C}.
