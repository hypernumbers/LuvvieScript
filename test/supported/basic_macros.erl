-module(basic_macros).

-compile(export_all).

-define(ARGYBARGY, 1).

macro_fn() ->
    ?ARGYBARGY.
