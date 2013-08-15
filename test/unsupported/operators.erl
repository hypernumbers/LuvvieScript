-module(operators).

-compile(export_all).

fn_bnot() ->
    2#10 bnot 2#01.

fn_band() ->
    2#10 band 2#01.

fn_bor() ->
    2#10 bor 2#01.

fn_bxor() ->
    2#10 bxor 2#01.

fn_bsl() ->
    2#10 bsl 2#01.

fn_bsr() ->
    2#10 bsr 2#01.
