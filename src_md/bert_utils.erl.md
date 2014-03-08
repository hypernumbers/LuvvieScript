-------------------------------------------------------------------
 @author    Gordon Guthrie
 @copyright (C) 2014, Gordon Guthrie
 @doc       Some utilities for using Bert to explore data types

 @end
 Created :  26 Feb 2014 by gordon@vixo.com
-------------------------------------------------------------------
```erlang
    -module(bert_utils).


    -export([
             all/0
            ]).

    all() ->
        Cases = [
                 {"integer",      12345},
                 {"float",        1.2345},
                 {"atom",         atom},
                 {"null",         []},
                 {"string",       "abcdef"},
                 {"tuple",        {a, 1, "X"}},
                 {"complex_list", [a, 1, "X"]}
                ],
        [write(Ident, Term) || {Ident, Term} <- Cases],
        ok.

    write(Identifier, Term) ->
        Binary = term_to_binary(Term),
        Json = io_lib:format("{~p: ~p};", [Identifier, Binary]),
        Json2 = re:replace(Json,  "<<", "\"", [{return, list}]),
        Json3 = re:replace(Json2, ">>", "\"", [{return, list}]),
        Dir = "../js/terms/",
        File = "terms.log",
        case file:open(Dir ++ File, [write, append]) of
            {ok, IODevice} ->
                io:fwrite(IODevice, "~s~n", [Json3]),
                file:close(IODevice);
            {error, Err} ->
                io:format("Error ~p opening file~n", [Err]),
                exit(banjo)
        end.
```
