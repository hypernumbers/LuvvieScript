-module(types).

-compile(export_all).

types() ->
    A  = 1,
    B  = 2.3,
    C1 = true,
    C2 = blue,
    D  = [a, b],
    E  = {1, 2},
    F  = "string",
    G = make_ref(),
    {A, B, C1, C2, D, E, F, G}.
