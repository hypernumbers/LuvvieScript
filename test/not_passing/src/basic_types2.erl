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

chars() ->
    [$n, $o, $w, $,, $\t, $i, $s, $\t, $t, $h, $e, $\t, $w, $i, $n, $t, $e, $r].

radishes() ->
    [2#10, 16#104, 32#4545].

exponents() ->
    [2.3e+9, 2.3e-9].
