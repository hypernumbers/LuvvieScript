   @author    Gordon Guthrie
   @copyright (C) 2013, Gordon Guthrie
   @doc       This is the luvviescript compiler

   @end
   Created : 17 Aug 2013 by gordon@vixo.com
```erlang
    -module(luvviescript).

    -export([
             compile/1,
             compile/2
            ]).

    -include("luvviescript.hrl").
```
 include the core erlang syntax records that we are going to act on
 this file is in /usr/local/lib/erlang/lib/compiler-N.N.N/src
 or the equivalent. That dir needs to be set in the rebar compiler
 options for this to compile
```erlang
    -include_lib("core_parse.hrl").

    -define(LINEENDING, {line_ending, ";", nonce}).
    -define(INDENT, "    ").

    compile(File) ->
        compile(File, production).

    compile(File, Environment) ->
        {ok, _, Syntax} = compile_to_ast(File),
        ok = maybe_write(Environment, File, Syntax, ".ast"),
        #c_module{defs = Body} = Syntax,
        %% Erlang regards somefun/1 and somefun/N as two differnt
        %% fns, Javascript thinks they are they same.
        %% so we need to group multiple arity erlang fns.
        %% easiest way to do that is to sort the fns by name/arity
        SortFn = fun({Var1, _}, {Var2, _}) ->
                         Var1#c_var.name < Var2#c_var.name
                 end,
        Body2 = lists:sort(SortFn, Body),
        ok = maybe_write(Environment, File, Syntax#c_module{defs = Body2}, ".ast2"),
        {ok, Src} = file:read_file(File),
        {ok, Tokens, _} = erl_scan:string(binary_to_list(Src), 1,
                                          [
                                           return_white_spaces,
                                           return_comments,
                                           text
                                          ]),
        ok = maybe_write(Environment, File, Tokens, ".tks"),
        {ok, Tokens2} = collect_tokens(Tokens),
        ok = maybe_write(Environment, File, Tokens2, ".tks2"),
        %% {Merged, _Cx, _Tokens3} = merge_and_conv(Syntax2, Tokens2, #context{}, []),
        %% ok = maybe_write(Environment, File, Merged, ".ast3"),
        ok.

    merge_and_conv([], Tks, Cx, Acc) ->
        {lists:reverse(Acc), Tks, Cx};
    merge_and_conv([{Fn, Contents} | T], Tks, Cx, Acc)
      when Fn =:= singleton_fn orelse
           Fn =:= multiarity_fn ->
        {NewContents, NewTks2, NewCx} = merge_and_conv(Contents, Tks, Cx, []),
        Js = to_js:conv({Fn, NewContents}, NewCx),
        merge_and_conv(T, NewTks2, NewCx, [Js | Acc]);
    merge_and_conv([{function, L, Fn, N, Contents} | T], Tks, Cx, Acc) ->
        {Details,     NewTks}  = get_details(Tks, L, {function, Fn}),
        {NewContents, NewTks2, NewCx} = merge_and_conv(Contents, NewTks, Cx, []),
        NewFn = {function, Details, Fn, N, NewContents},
        Js = to_js:conv(NewFn, NewCx),
        merge_and_conv(T, NewTks2, NewCx, [Js | Acc]);
    merge_and_conv([{cons, L, Head, Tail} | T], Tks, Cx, Acc) ->
        {NewHead, NewTks,  NewCx}  = merge_and_conv([Head], Tks, Cx, []),
        {NewTail, NewTks2, NewCx2} = merge_and_conv([Tail], NewTks, NewCx, []),
        NewCons = {cons, {L, none}, NewHead, NewTail},
        Js = to_js:conv(NewCons, NewCx2),
        merge_and_conv(T, NewTks2, NewCx2, [Js | Acc]);
    merge_and_conv([{match, L, Left, Right} | T], Tks, Cx, Acc) ->
        {Details, TksA} = get_details(Tks, L, {match, "="}),
        {NewLeft,  NewTks,  NewCx}  = merge_and_conv([Left], TksA,    Cx,    []),
        {NewRight, NewTks2, NewCx2} = merge_and_conv([Right], NewTks, NewCx, []),
        Match = {match, Details, NewLeft, NewRight},
        Js = to_js:conv(Match, NewCx2),
        merge_and_conv(T, NewTks2, NewCx2, [Js | Acc]);
    merge_and_conv([{op, L, Type, Expr} | T], Tks, Cx, Acc)
      when Type =:= '+' orelse % unary plus
           Type =:= '-' ->     % unary minus
        {Details, NewTks} = get_details(Tks, L, {op, Type}),
        {NewExpr, NewTks2, NewCx} = merge_and_conv([Expr], NewTks,  Cx, []),
        NewOp = {op, Details, Type, NewExpr},
        Js = to_js:conv(NewOp, NewCx),
        merge_and_conv(T, NewTks2, NewCx, [Js | Acc]);
    merge_and_conv([{op, L, Type, LExpr, RExpr} | T], Tks, Cx, Acc)
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
        {NewLExpr, NewTks,  NewCx}  = merge_and_conv([LExpr], NewTksA, Cx,    []),
        {NewRExpr, NewTks2, NewCx2} = merge_and_conv([RExpr], NewTks,  NewCx, []),
        NewOp = {op, Details, Type, NewLExpr, NewRExpr},
        Js = to_js:conv(NewOp, NewCx2),
        merge_and_conv(T, NewTks2, NewCx2, [Js | Acc]);
    merge_and_conv([{Type, L, Val} | T], Tks, Cx, Acc)
      when Type =:= char    orelse
           Type =:= float   orelse
           Type =:= var     orelse
           Type =:= integer orelse
           Type =:= string  orelse
           Type =:= atom    ->
        {Details, NewTks} = get_details(Tks, L, {Type, Val}),
        NewConst = {Type, Details, Val},
        Js = to_js:conv(NewConst, Cx),
        merge_and_conv(T, NewTks, Cx, [Js | Acc]);
    merge_and_conv([{tuple, L, Vals} | T], Tks, Cx, Acc) ->
        {NewVals, NewTks, NewCx} = merge_and_conv(Vals, Tks, Cx, []),
        NewTuple = {tuple, {L, none}, NewVals},
        Js = to_js:conv(NewTuple, NewCx),
        merge_and_conv(T, NewTks, NewCx, [Js | Acc]);
    merge_and_conv([{'if', L, Clauses} | T], Tks, Cx, Acc) ->
        {Details, NewTks} = get_details(Tks, L, {'if', 'if'}),
        {NewClauses, NewTks2, NewCx} = merge_and_conv(Clauses, NewTks, Cx, []),
        NewIf = {'if', Details, NewClauses},
        Js = to_js:conv(NewIf, NewCx),
        merge_and_conv(T, NewTks2, NewCx, [Js | Acc]);
    merge_and_conv([{'case', L, Expr, Clauses} | T], Tks, Cx, Acc) ->
        {NewExpr,    NewTks,  NewCx} = merge_and_conv([Expr], Tks,       Cx,    []),
        {NewClauses, NewTks2, NewCx2} = merge_and_conv(Clauses, NewTks,  NewCx, []),
        NewCase = {'case', {L, none}, NewExpr, NewClauses},
        Js = to_js:conv(NewCase, NewCx2),
        merge_and_conv(T, NewTks2, NewCx, [Js | Acc]);
    merge_and_conv([{clauses, Clauses} | T], Tks, Cx, Acc) ->
        {NewClauses, NewTks, NewCx} = merge_and_conv(Clauses, Tks, Cx, []),
        NewC = {clauses, NewClauses},
        Js = to_js:conv(NewC, NewCx),
        merge_and_conv(T, NewTks, NewCx, [Js | Acc]);
    merge_and_conv([{clause, L, Args, Guards, Contents} | T], Tks, Cx, Acc) ->
        {NewArgs, NewTks, NewCx}  = merge_and_conv(Args, Tks, Cx, []),
        Fun = fun(X, {Gs, Tokens, Context}) ->
                      {NewG, NewTokens, NewC} = merge_and_conv(X, Tokens, Context, []),
                      {[NewG | Gs], NewTokens, NewC}
              end,
        {NewGuards, NewTks2, NewCx2} = lists:foldl(Fun, {[], NewTks, NewCx}, Guards),
        {NewCnts, NewTks3, NewCx3} = merge_and_conv(Contents, NewTks2, NewCx2, []),
        NewClause = {clause, {L, none}, NewArgs, [NewGuards], NewCnts},
        Js = to_js:conv(NewClause, NewCx3),
        merge_and_conv(T, NewTks3, NewCx2, [Js | Acc]);
    merge_and_conv([{eof, _L} | T], Tks, Cx, Acc) ->
        %% discard eof
        merge_and_conv(T, Tks, Cx, Acc);
    merge_and_conv([{nil, L} | T], Tks, Cx, Acc) ->
        NewNil = {nil, {L, none}},
        Js = to_js:conv(NewNil, Cx),
        merge_and_conv(T, Tks, Cx, [Js | Acc]);
    merge_and_conv([{attribute, L, A, B} | T], Tks, Cx, Acc) ->
        NewA = {attributes, {L, none}, A, B},
        Js = to_js:conv(NewA, Cx),
        merge_and_conv(T, Tks, Cx, [Js | Acc]);
    merge_and_conv([{lc, L, Statement, Generators} | T], Tks, Cx, Acc) ->
        {NewSt,   NewTks,  NewCx}  = merge_and_conv([Statement], Tks,    Cx, []),
        {NewGens, NewTks2, NewCx2} = merge_and_conv(Generators,  NewTks, NewCx, []),
        NewLC = {lc, {L, none}, NewSt, NewGens},
        Js = to_js:conv(NewLC, NewCx2),
        merge_and_conv(T, NewTks2, NewCx2, [Js | Acc]);
    merge_and_conv([{generate, L, Stment1, Stment2} | T], Tks, Cx, Acc) ->
        {NewSt1, NewTks1, NewCx}  = merge_and_conv([Stment1], Tks,     Cx,    []),
        {NewSt2, NewTks2, NewCx2} = merge_and_conv([Stment2], NewTks1, NewCx, []),
        NewG = {generate, {L, none}, NewSt1, NewSt2},
        Js = to_js:conv(NewG, NewCx2),
        merge_and_conv(T, NewTks2, NewCx2, [Js | Acc]);
    merge_and_conv([{call, L, Fn} | T], Tks, Cx, Acc) ->
        {NewFn, NewTks, NewCx} = merge_and_conv([Fn], Tks, Cx, []),
        NewC = {call, {L, none}, NewFn},
        Js = to_js:conv(NewC, NewCx),
        merge_and_conv(T, NewTks, NewCx, [Js | Acc]);
    merge_and_conv([{'fun', L, Clauses} | T], Tks, Cx, Acc) ->
        {NewClauses, NewTks, NewCx} = merge_and_conv([Clauses], Tks, Cx, []),
        NewFn = {call, {L, none}, NewClauses},
        Js = to_js:conv(NewFn, NewCx),
        merge_and_conv(T, NewTks, NewCx, [Js | Acc]);
    merge_and_conv([{call, L, Fn, Args} | T], Tks, Cx, Acc) ->
        {NewFn,   NewTks,  NewCx}  = merge_and_conv([Fn], Tks,    Cx,    []),
        {NewArgs, NewTks2, NewCx2} = merge_and_conv(Args, NewTks, NewCx, []),
        NewC = {call, {L, none}, NewFn, NewArgs},
        Js = to_js:conv(NewC, NewCx2),
        merge_and_conv(T, NewTks2, NewCx2, [Js | Acc]);
    merge_and_conv([{record, L, A, B} | T], Tks, Cx, Acc) ->
        NewR = {record, {L, none}, A, B},
        Js = to_js:conv(NewR, Cx),
        merge_and_conv(T, Tks, Cx, [Js | Acc]);
    merge_and_conv([{record_field, L, A, B, C} | T], Tks, Cx, Acc) ->
        NewR = {record_field, {L, none}, A, B, C},
        Js = to_js:conv(NewR, Cx),
        merge_and_conv(T, Tks, Cx, [Js | Acc]);
    merge_and_conv([{record_index, L, A, B} | T], Tks, Cx, Acc) ->
        NewR = {record_index, {L, none}, A, B},
        Js = to_js:conv(NewR, Cx),
        merge_and_conv(T, Tks, Cx, [Js | Acc]).
```
 merge_and_conv([H | T], Tks, Acc) ->
     io:format("H is ~p~n", [H]),
     merge_and_conv(T, Tks, [H | Acc]).

```erlang
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
        IncludeDir = filename:dirname(File) ++ "../include",
        compile:file(File, [{i, IncludeDir}, binary, to_core]).

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
        %% if whitespace starts with a new line it is a terminal whitespace
        %% so reset indent counter to it (ie don't add it to Indent)
        %% otherwise business as usual...
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
        End = Indent + length(Txt),
        {{Ln, {Indent, End}}, End + 1}.

    maybe_write(production, _, _, _) ->
        ok;
    maybe_write(debug, File, Contents, FileType) ->
        OutputDir = filename:dirname(File) ++ "/../debug/",
        OutputFile = filename:rootname(filename:basename(File)) ++ FileType,
        _Return = filelib:ensure_dir(File),
        ok = make_utils:write_file(Contents, OutputDir ++ OutputFile).
```
