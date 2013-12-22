-module(basic_types).

-compile(export_all).

int_fn() ->
    A = 1,
    A.

float_fn() ->
    B = 2.3,
    B.

boolean_fn() ->
    C = true,
    C.

atom1_fn() ->
    D = blue,
    D.

atom2_fn() ->
    E = 'Blue 4 U',
    E.

string_fn() ->
    F = "string theory",
    F.


bish_fn(A) ->
    case A of
        bash -> berk;
        bosh -> bork
    end.
