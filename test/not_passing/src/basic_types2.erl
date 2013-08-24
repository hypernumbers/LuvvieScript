-module(basic_types2).

-compile(export_all).

list() ->
    A  = [a, b],
    A.

tuple() ->
    B  = {1, 2},
    B.

ref() ->
    C = make_ref(),
    C.
