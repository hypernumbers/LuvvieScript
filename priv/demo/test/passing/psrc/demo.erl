-file("test/passing/src/demo.erl", 1).

-module(demo).

-export([test/0]).

test() ->
    A = first(),
    B = second(),
    C = third(),
    A + B / C.

first() ->
    1.

second() ->
    2.

third() ->
    3.



