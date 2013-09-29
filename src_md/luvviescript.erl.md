   @author    Gordon Guthrie
   @copyright (C) 2013, Gordon Guthrie
   @doc       This is the luvviescript compiler

   @end
   Created : 17 Aug 2013 by gordon@vixo.com
```erlang
    -module(luvviescript).

    -export([
             compile/1,
             compile/2,
             pretty_print/1
            ]).

    -include("luvviescript.hrl").

    -define(LINEENDING, {line_ending, ";", nonce}).
    -define(INDENT, "    ").

```
```
  -define(INDENT, "zzzz").

```erlang
    compile(File) ->
        compile(File, production).

    compile(File, Environment) ->
        {ok, Syntax} = compile_to_ast(File),
        ok = maybe_write(Environment, File, Syntax, ".ast"),
        {ok, Binary} = read_dot_P_file(File),
        {ok, Tokens, _} = erl_scan:string(binary_to_list(Binary), 1,
                                          [
                                           return_white_spaces,
                                           return_comments,
                                           text
                                          ]),
        ok = maybe_write(Environment, File, Tokens, ".tks"),
        {ok, Tokens2} = collect_tokens(Tokens),
        ok = maybe_write(Environment, File, Tokens2, ".tks2"),
        {_Tokens3, Merged} = merge(Syntax, Tokens2, []),
        ok = maybe_write(Environment, File, Merged, ".ast2"),
        ok.

    merge([], Tokens, Acc) ->
        {Tokens, lists:reverse(Acc)};
    merge([{function, Line, Fn, N, Contents} | T], Tokens, Acc) ->
        {Details, NewTokens} = get_details(Tokens, Line, {function, Fn}),
        {NewContents, NewTokens2} = merge(Contents, NewTokens, []),
        NewFn = {function, Details, Fn, N, NewContents},
        merge(T, NewTokens2, [NewFn | Acc]);
    merge([{cons, Line, Head, Tail} | T], Tokens, Acc) ->
        {NewHead, NewTokens}  = merge([Head], Tokens,    []),
        {NewTail, NewTokens2} = merge([Tail], NewTokens, []),
        NewCons = {cons, {Line, none}, NewHead, NewTail},
        merge(T, NewTokens2, [NewCons | Acc]);
    merge([{Type, Line, Val} | T], Tokens, Acc)
      when Type =:= char    orelse
           Type =:= float   orelse
           Type =:= var     orelse
           Type =:= integer orelse
           Type =:= string ->
        {Details, NewTokens} = get_details(Tokens, Line, {Type, Val}),
        merge(T, NewTokens, [{Type, Details, Val} | Acc]);
    merge([{clause, L, Args, Guards, Contents} | T], Tokens, Acc) ->
        {NewArgs,     NewTokens}  = merge(Args,     Tokens,     []),
        {NewGuards,   NewTokens2} = merge(Guards,   NewTokens,  []),
        {NewContents, NewTokens3} = merge(Contents, NewTokens2, []),
        NewClause = {clause, {L, none}, NewArgs, NewGuards, NewContents},
        merge(T, NewTokens3, [NewClause | Acc]);
    merge([{Type, L} | T], Tokens, Acc)
      when Type =:= eof orelse
           Type =:= nil ->
        merge(T, Tokens, [{Type, {L, none}} | Acc]);
    merge([{attribute, L, A, B} | T], Tokens, Acc) ->
        merge(T, Tokens, [{attributes, {L, 0}, A, B} | Acc]);
    merge([H | T], Tokens, Acc) ->
        io:format("H is ~p~n", [H]),
        merge(T, Tokens, [H | Acc]).

    get_details(Tokens, Ln, {_Type, Name}) ->
        case lists:keyfind(Ln, 1, Tokens) of
            false ->
                io:format("no tokens for ~p (~p}~n", [Ln, Name]),
                {{Ln, none}, Tokens};
            {Ln, Tks} ->
                io:format("Tks is ~p~nName is ~p~n", [Tks, Name]),
                case lists:keyfind(Name, 1, Tks) of
                    false ->
                        {{Ln, none}, Tokens};
                    {Name, Details, _} ->
                        NewTks = lists:keydelete(Name, 1, Tks),
                        NewTokens = lists:keystore(Ln, 1, Tokens, {Ln, NewTks}),
                        {Details, NewTokens}
                end
        end.

    read_dot_P_file(File) ->
        PDir = filename:dirname(File) ++ "/../psrc/",
        File2 = filename:rootname(filename:basename(File)) ++ ".P",
        file:read_file(PDir ++ File2).

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


    collect_tokens(List) -> {ok, col2(List, 1, 1, [], [])}.

    col2([], Line, Indent,  A1, A2) ->
        {Entry, _NewIndent} = make_entry(lists:reverse(A1), Indent, []),
        lists:reverse([{Line, Entry} | A2]);
    col2([H  | T], Line, Indent, A1, A2) ->
        Details = element(2, H),
        Ln = get_line(Details),
        case Line of
            Ln -> col2(T, Ln, Indent, [H | A1], A2);
            _  -> {Entry, NewIndent} = make_entry(lists:reverse(A1), Indent, []),
                  col2([H | T], Ln, NewIndent, [], [{Line, Entry} | A2])
        end.

    make_entry([], Indent, Acc) ->
        {lists:reverse(Acc), Indent};
    make_entry([{Operator, Details} | T], Indent, Acc) ->
        {Loc, NewIndent} = make_location(Details, Indent),
        NewAcc = {Operator, Loc, operator},
        make_entry(T, NewIndent, [NewAcc | Acc]);
    make_entry([{white_space, Details, WS} | T], Indent, Acc) ->
        % if whitespace starts with a new line it is a terminal whitespace
        % so reset indent counter to it (ie don't add it to Indent)
        % otherwise business as usual...
        [{line, Line}, {text, Txt}] = Details,
        [H | _] = Txt,
        NewIndent = case H of
                        $\n -> length(Txt);
                         _  -> length(Txt) + Indent
                    end,
        NewAcc = {WS, {Line, Indent}, white_space},
        make_entry(T, NewIndent, [NewAcc | Acc]);
```
 comments are either whole line (don't care about the length)
 or at the end of a line followed by whitespace (don't care about the length).
```erlang
    make_entry([{comment, _, _} | T], Indent, Acc) ->
        make_entry(T, Indent, Acc);
    make_entry([{Type, Details, Thing} | T], Indent, Acc) ->
        {Loc, NewIndent} = make_location(Details, Indent),
        NewAcc = {Thing, Loc, Type},
        make_entry(T, NewIndent, [NewAcc | Acc]).

    get_line([{line, Ln}, _]) -> Ln.

    make_location([{line, Ln}, {text, Txt}], Indent) ->
        {{Ln, Indent}, Indent + length(Txt)}.

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

    maybe_write(production, _, _, _) ->
        ok;
    maybe_write(debug, File, Contents, FileType) ->
        OutputDir = filename:dirname(File) ++ "/../debug/",
        OutputFile = filename:rootname(filename:basename(File)) ++ FileType,
        _Return = filelib:ensure_dir(File),
        ok = make_utils:write_file(Contents, OutputDir ++ OutputFile).

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
```
