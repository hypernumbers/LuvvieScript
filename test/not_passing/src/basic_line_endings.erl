-module(basic_line_endings).

-compile(export_all).

single() -> A = 1, B = 3, A + B.

complex_expressions() ->
    A = 1,
    B = 2,
    C = (88 * 4) + B/A,
    C/2.
