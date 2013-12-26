-module('1b_simpler').

-export([
         simple_fn/0
        ]).

simple_fn() ->
    not_exported_fn().

not_exported_fn() ->
    django.
