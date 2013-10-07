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

    compile(File) ->
        compile(File, production).

    compile(File, Environment) ->
        {ok, DotP2} = make_dot_P2(File),
        {ok, Syntax} = compile_to_ast(File),
        ok = maybe_write(Environment, File, Syntax, ".ast"),
        {ok, Tokens, _} = erl_scan:string(lists:flatten(DotP2), 1,
                                          [
                                           return_white_spaces,
                                           return_comments,
                                           text
                                          ]),
        ok = maybe_write(Environment, File, Tokens, ".tks"),
        {ok, Tokens2} = collect_tokens(Tokens),
        ok = maybe_write(Environment, File, Tokens2, ".tks2"),
        {Merged, _Tokens3} = merge(Syntax, Tokens2, []),
        ok = maybe_write(Environment, File, Merged, ".ast2"),
        ok.

    merge([], Tks, Acc) ->
        {lists:reverse(Acc), Tks};
    merge([{function, L, Fn, N, Contents} | T], Tks, Acc) ->
        {Details,     NewTks}  = get_details(Tks, L, {function, Fn}),
        {NewContents, NewTks2} = merge(Contents, NewTks, []),
        NewFn = {function, Details, Fn, N, NewContents},
        merge(T, NewTks2, [NewFn | Acc]);
    merge([{cons, L, Head, Tail} | T], Tks, Acc) ->
        {NewHead, NewTks}  = merge([Head], Tks, []),
        {NewTail, NewTks2} = merge([Tail], NewTks, []),
        NewCons = {cons, {L, none}, NewHead, NewTail},
        merge(T, NewTks2, [NewCons | Acc]);
    merge([{match, L, Left, Right} | T], Tks, Acc) ->
        {Details, TksA} = get_details(Tks, L, {match, "="}),
        {NewLeft,  NewTks}  = merge([Left], TksA, []),
        {NewRight, NewTks2} = merge([Right], NewTks, []),
        Match = {match, Details, NewLeft, NewRight},
        merge(T, NewTks2, [Match | Acc]);
    merge([{op, L, Type, Expr} | T], Tks, Acc)
      when Type =:= '+' orelse % unary plus
           Type =:= '-' ->     % unary minus
        {Details, NewTks} = get_details(Tks, L, {op, Type}),
        {NewExpr, NewTks2} = merge([Expr], NewTks,  []),
        merge(T, NewTks2, [{op, Details, Type, NewExpr} | Acc]);
    merge([{op, L, Type, LExpr, RExpr} | T], Tks, Acc)
      when Type =:= '+'       orelse
           Type =:= '-'       orelse
           Type =:= '/'       orelse
           Type =:= '*'       orelse
           Type =:= 'rem'     orelse
           Type =:= 'div'     orelse
           Type =:= '=='      orelse
           Type =:= '/='      orelse
           Type =:= '=:='     orelse
           Type =:= '=/='     orelse
           Type =:= '>'       orelse
           Type =:= '<'       orelse
           Type =:= '=<'      orelse
           Type =:= '>='      orelse
           Type =:= 'orelse'  orelse
           Type =:= 'andalso' ->
        {Details, NewTksA} = get_details(Tks, L, {op, Type}),
        {NewLExpr, NewTks} = merge([LExpr], NewTksA,  []),
        {NewRExpr, NewTks2} = merge([RExpr], NewTks,  []),
        NewOp = {op, Details, Type, NewLExpr, NewRExpr},
        merge(T, NewTks2, [NewOp | Acc]);
    merge([{Type, L, Val} | T], Tks, Acc)
      when Type =:= char    orelse
           Type =:= float   orelse
           Type =:= var     orelse
           Type =:= integer orelse
           Type =:= string  orelse
           Type =:= atom    ->
        {Details, NewTks} = get_details(Tks, L, {Type, Val}),
        merge(T, NewTks, [{Type, Details, Val} | Acc]);
    merge([{tuple, L, Vals} | T], Tks, Acc) ->
        {NewVals, NewTks} = merge(Vals, Tks,  []),
        merge(T, NewTks, [{tuple, {L, none}, NewVals} | Acc]);
    merge([{'if', L, Clauses} | T], Tks, Acc) ->
        {Details, NewTks} = get_details(Tks, L, {'if', 'if'}),
        {NewClauses, NewTks2} = merge(Clauses, NewTks,  []),
        merge(T, NewTks2, [{'if', Details, NewClauses} | Acc]);
    merge([{'case', L, Expr, Clauses} | T], Tks, Acc) ->
        {NewExpr, NewTks} = merge([Expr], Tks,  []),
        {NewClauses, NewTks2} = merge(Clauses, NewTks,  []),
        NewCase = {'case', {L, none}, NewExpr, NewClauses},
        merge(T, NewTks2, [NewCase | Acc]);
    merge([{clauses, Clauses} | T], Tks, Acc) ->
        {NewClauses, NewTks} = merge(Clauses, Tks, []),
        NewC = {clauses, NewClauses},
        merge(T, NewTks, [NewC | Acc]);
    merge([{clause, L, Args, Guards, Contents} | T], Tks, Acc) ->
        {NewArgs, NewTks}  = merge(Args, Tks,  []),
        Fun = fun(X, {Gs, Tokens}) ->
                      {NewG, NewTokens} = merge(X, Tokens, []),
                      {[NewG | Gs], NewTokens}
              end,
        {NewGuards,   NewTks2} = lists:foldl(Fun, {[], NewTks}, Guards),
        {NewContents, NewTks3} = merge(Contents, NewTks2, []),
        NewClause = {clause, {L, none}, NewArgs, [NewGuards], NewContents},
        merge(T, NewTks3, [NewClause | Acc]);
    merge([{Type, L} | T], Tks, Acc)
      when Type =:= eof orelse
           Type =:= nil ->
        merge(T, Tks, [{Type, {L, none}} | Acc]);
    merge([{attribute, L, A, B} | T], Tks, Acc) ->
        merge(T, Tks, [{attributes, {L, none}, A, B} | Acc]);
    merge([{lc, L, Statement, Generators} | T], Tks, Acc) ->
        {NewSt,   NewTks}  = merge([Statement], Tks, []),
        {NewGens, NewTks2} = merge(Generators,  NewTks, []),
        NewLC = {lc, {L, none}, NewSt, NewGens},
        merge(T, NewTks2, [NewLC | Acc]);
    merge([{generate, L, Statement1, Statement2} | T], Tks, Acc) ->
        {NewSt1, NewTks1} = merge([Statement1], Tks,     []),
        {NewSt2, NewTks2} = merge([Statement2], NewTks1, []),
        NewG = {generate, {L, none}, NewSt1, NewSt2},
        merge(T, NewTks2, [NewG | Acc]);
    merge([{call, L, Fn} | T], Tks, Acc) ->
        {NewFn, NewTks} = merge([Fn], Tks, []),
        NewC = {call, {L, none}, NewFn},
        merge(T, NewTks, [NewC | Acc]);
    merge([{'fun', L, Clauses} | T], Tks, Acc) ->
        {NewClauses, NewTks} = merge([Clauses], Tks, []),
        NewFn = {call, {L, none}, NewClauses},
        merge(T, NewTks, [NewFn | Acc]);
    merge([{call, L, Fn, Args} | T], Tks, Acc) ->
        {NewFn,   NewTks}  = merge([Fn], Tks,    []),
        {NewArgs, NewTks2} = merge(Args, NewTks, []),
        NewC = {call, {L, none}, NewFn, NewArgs},
        merge(T, NewTks2, [NewC | Acc]);
    merge([{record, L, A, B} | T], Tks, Acc) ->
        merge(T, Tks, [{record, {L, none}, A, B} | Acc]);
    merge([{record_field, L, A, B, C} | T], Tks, Acc) ->
        merge(T, Tks, [{record_field, {L, none}, A, B, C} | Acc]);
    merge([{record_index, L, A, B} | T], Tks, Acc) ->
        merge(T, Tks, [{record_index, {L, none}, A, B} | Acc]);
    merge([H | T], Tks, Acc) ->
        io:format("H is ~p~n", [H]),
        merge(T, Tks, [H | Acc]).

    get_details(Tokens, Ln, {_Type, Name}) ->
        case lists:keyfind(Ln, 1, Tokens) of
            false ->
                {{Ln, none}, Tokens};
            {Ln, Tks} ->
                case lists:keyfind(Name, 1, Tks) of
                    false ->
                        {{Ln, none}, Tokens};
                    {Name, Details, _} ->
                        NewTks = lists:keydelete(Name, 1, Tks),
                        NewTokens = lists:keystore(Ln, 1, Tokens, {Ln, NewTks}),
                        {Details, NewTokens}
                end
        end.

    make_dot_P2(File) ->
        IncludeDir = filename:dirname(File) ++ "/../include",
        PDir       = filename:dirname(File) ++ "/../psrc",
        case compile:file(File, [
                                 'P',
                                 {i,      IncludeDir},
                                 {outdir, PDir}
                                ]) of
            {ok, []} -> File2 = filename:rootname(filename:basename(File)) ++ ".P",
                        {ok, P2} = case file:open(PDir ++ "/" ++ File2, read) of
                                       {error, Err} -> exit(Err);
                                       {ok, ID}     -> FileNameFlag = false,
                                                       scan(ID, FileNameFlag, [])
                                   end,
                        File3 = filename:rootname(filename:basename(File)) ++ ".P2",
                        ok = write_file(P2, PDir ++ "/" ++ File3),
                        {ok, P2};
            error    -> io:format("Cannae compile...~n"),
                        {error, cant_compile}
        end.

    write_file(List, File) ->
        _Ret = filelib:ensure_dir(File),
        case file:open(File, [append]) of
            {ok, Id} -> write_f2(List, Id),
                        file:close(Id),
                        ok;
            _        -> error
        end,
        ok.

    write_f2([],      _Id) -> ok;
    write_f2([H | T], Id)  -> io:fwrite(Id, "~s", [H]),
                              write_f2(T, Id).

    scan(ID, Flag, Acc) ->
        case {file:read_line(ID), Flag} of
            {{ok, "-file(" ++ _Rest = F}, false} -> scan(ID, true, [F | Acc]);
            {{ok, "-file(" ++ _Rest},     true}  -> scan(ID, Flag, Acc);
            {{ok, Line},                  _}     -> scan(ID, Flag, [Line | Acc]);
            {eof,                         _}     -> Rev = lists:reverse(Acc),
                                                    {ok, Rev}
        end.

    compile_to_ast(File) ->
        File2 = filename:rootname(filename:basename(File)) ++ ".P2",
        IncludeDir = [],
        PDir  = filename:dirname(File) ++ "/../psrc/",
        File3 = PDir ++ File2,
        epp:parse_file(File3, IncludeDir, []).

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
        {{Ln, Indent}, Indent + length(Txt) - 1}.

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
