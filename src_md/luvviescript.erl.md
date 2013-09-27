  @author    Gordon Guthrie
  @copyright (C) 2013, Gordon Guthrie
  @doc       This is the luvviescript compiler

  @end
  Created : 17 Aug 2013 by gordon@vixo.com
```erlang
    -module(luvviescript).

    -export([
             compile/1,
             pretty_print/1
            ]).

    -include("luvviescript.hrl").

    -define(LINEENDING, {line_ending, ";", nonce}).
    -define(INDENT, "    ").

```
 -define(INDENT, "zzzz").

```erlang
    compile(File) ->
        {ok, Syntax}    = compile_to_ast(File),
        {ok, Binary}    = file:read_file(File),
        {ok, Tokens, _} = erl_scan:string(binary_to_list(Binary), 1,
                                          [return_white_spaces, return_comments]),
        {ok, Tokens2}  = collect_tokens(Tokens),
        io:format("Tokens2 is ~p~n", [Tokens2]),
        ok.

    compile_to_ast(File) ->
        IncludeDir = filename:dirname(File) ++ "/../include",
        OutDir     = filename:dirname(File) ++ "/../psrc",
        case compile:file(File, [
                                 'P',
                                 {i,      IncludeDir},
                                 {outdir, OutDir}
                                ]) of
            {ok, []} -> File2 = filename:rootname(filename:basename(File)) ++ ".P",
                        File3 = OutDir ++ "/" ++ File2,
                        epp:parse_file(File3, IncludeDir, []);
            error    -> io:format("Cannae compile...~n"),
                        {error, File}
        end.


    collect_tokens(List) -> {ok, col2(List, 1, [], [])}.

    col2([],       N,  A1, A2) -> lists:reverse([{N, lists:reverse(A1)} | A2]);
    col2([H  | T], N2, A1, A2) -> N1 = element(2, H),
                                  case N2 of
                                      N1 -> col2(T, N1, [H | A1], A2);
                                      _  -> col2([H | T], N1, [], [{N2, lists:reverse(A1)} | A2])
                                  end.

    print_contents(C) -> print_c(C, []).

    print_c([],                   Acc) -> lists:flatten(lists:reverse(Acc));
    print_c([{_, String, _} | T], Acc) -> print_c(T, [String | Acc]).

    pretty_print(Rec) when is_record(Rec, module) ->
        io:format("Module:~n" ++
                      "-Name       : ~p~n" ++
                      "-Attributes : ~p~n" ++
                      "-Includes   : ~p~n" ++
                      "-Records    : ~p~n",
                  [
                   Rec#module.name,
                   Rec#module.attributes,
                   Rec#module.includes,
                   Rec#module.records
                  ]),
        io:format("Contents of Module:~n"),
        [pretty_print(X) || X <- Rec#module.contents],
        ok;
    pretty_print(Rec) when is_record(Rec, function) ->
        io:format("Function:~n" ++
                      "-Name    : ~p~n" ++
                      "-Arity   : ~p~n" ++
                      "-Line No : ~p~n",
                  [
                   Rec#function.name,
                   Rec#function.arity,
                   Rec#function.line_no
                  ]),
        io:format("Contents of function:~n"),
        [pretty_print(X) || X <- Rec#function.contents],
        ok;
    pretty_print(Rec) when is_record(Rec, clause) ->
        io:format("Clause:~n" ++
                      "-Params : ~p~n" ++
                      "-Guards : ~p~n" ++
                      "-LineNo : ~p~n",
                  [
                   Rec#clause.params,
                   Rec#clause.guards,
                   Rec#clause.line_no
                  ]),
        io:format("Clause contains~n"),
        [io:format("~p~n", [X]) || X <- Rec#clause.contents],
        ok.

    indent(N) when N < 0 ->
        integer_to_list(N) ++ "xxxx";
    indent(N) ->
        lists:flatten(lists:duplicate(N, ?INDENT)).

    plain_log(String, File) ->
        _Return = filelib:ensure_dir(File),

        case file:open(File, [append]) of
            {ok, Id} ->
                io:fwrite(Id, "~s~n", [String]),
                file:close(Id);
            _ ->
                error
        end.


```
