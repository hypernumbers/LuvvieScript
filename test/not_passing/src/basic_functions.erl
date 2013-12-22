-module(basic_functions).

%%% this module is designed to test the rejigging of functions that have one clause

-export([
         zero_arity/0
        ]).

zero_arity() ->
    A = arity(a),
    B = arity("B", 2),
    C = arity(3, 5, "C33"),
    {A, B, C}.

arity(_One, _Two, _Three) ->
    berk.

arity(erko) ->
    limbo;
arity("one") ->
    rhubarbo;
arity(One) when is_list(One) ->
    interpolate(One);
arity(_One) ->
    erko.

interpolate(_Yando) ->
    smando.

arity(one, _Two) ->
    shambo;
arity("One", _Two) ->
    bambo.
