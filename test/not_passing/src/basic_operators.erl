-module(basic_operators).

-compile(export_all).

plus() ->
    1 + 2.

unary_plus() ->
    + 4.

minus() ->
    1 - 2.

unary_minus() ->
    - 3.

times() ->
    3 * 5.

floating_div() ->
    3/4.

int_div() ->
    7 div 3.

remainder() ->
    7 rem 3.
