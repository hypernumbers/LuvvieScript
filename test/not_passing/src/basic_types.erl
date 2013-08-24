-module(basic_types).

-compile(export_all).

int() ->
    A = 1,
    A.

float() ->
    B = 2.3,
    B.

boolean() ->
    C = true,
    C.

atom1() ->
    D = blue,
    D.

atom2() ->
    E = 'Blue 4 U',
    E.

string() ->
    F = "string theory",
    F.
