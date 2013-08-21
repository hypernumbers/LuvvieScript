%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       This is the luvviescript compiler
%%%
%%% @end
%%% Created : 17 Aug 2013 by gordon@vixo.com
-module(luvviescript).

-export([
         compile/1,
         pretty_print/1
        ]).

-record(module, {
          name,
          attributes,
          includes,
          records,
          contents
         }).

-record(function, {
          name,
          arity,
          line_no,
          contents = []
         }).

-record(clause, {
          params,
          guards,
          line_no,
          contents
         }).

-define(INDENT, 4).

compile(File) ->
    {ok, Syntax} = compile_to_ast(File),
    io:format("Syntax is ~p~n", [Syntax]),
    Str = io_lib:format("~p~n", [Syntax]),
    plain_log(Str, "/tmp/syntax.log"),
    {ok, Binary}    = file:read_file(File),
    {ok, Tokens, _} = erl_scan:string(binary_to_list(Binary), 1,
                                      [return_white_spaces, return_comments]),
    io:format("Tokens is ~p~n", [Tokens]),
    Str2 = io_lib:format("~p~n", [Tokens]),
    plain_log(Str2, "/tmp/tokens.log"),
    {ok, _Tokens2} = collect_tokens(Tokens),
    Name = filename:rootname(filename:basename(File)) ++ ".erl",
    Output = comp2(Syntax, #module{name = Name}, Name, []),
    Str3 = io_lib:format("~p~n", [Output]),
    plain_log(Str3, "/tmp/output.log"),
    Output.

comp2([], Mod, _OrigF, Acc) ->
    Mod#module{contents = lists:reverse(Acc)};
comp2([{function, LineNo, Fn, Arity, Contents} | T], Mod, OrigF, Acc) ->
    C = comp_fn(Contents, OrigF, []),
    NewA = #function{name     = Fn,
                     arity    = Arity,
                     line_no  = LineNo,
                     contents = C},
    comp2(T, Mod, OrigF, [NewA | Acc]);
comp2([{eof, _} | T], Mod, OrigF, Acc) ->
    comp2(T, Mod, OrigF, Acc);
comp2([{attribute, _, file, {Name, _}} | T], Mod, _OrigF, Acc) ->
    io:format("Name is ~p~n", [Name]),
    Name2 = filename:basename(Name),
    comp2(T, Mod, Name2, Acc);
comp2([{attribute, _, record, Record} | T], Mod, OrigF, Acc) ->
    #module{records = Rs} = Mod,
    NewM = Mod#module{records = [Record | Rs]},
    comp2(T, NewM, OrigF, Acc);
comp2([{attribute, _, _, _} = A | T], Mod, OrigF, Acc) ->
    io:format("A is ~p~n", [A]),
    #module{attributes = Attrs} = Mod,
    NewM = Mod#module{attributes = [A | Attrs]},
    comp2(T, NewM, OrigF, Acc);
comp2([H | T], Mod, OrigF, Acc) ->
    io:format("Handle ~p~n", [H]),
    comp2(T, Mod, OrigF, Acc).

comp_fn([], _OrigF, Acc) ->
    lists:reverse(Acc);
comp_fn([{clause, LineNo, Params, Guards, Contents} | T], OrigF, Acc) ->
    C = comp_st(Contents, clause, OrigF, []),
    io:format(print_contents(C) ++ "~n"),
    NewAcc = #clause{params   = Params,
                     guards   = Guards,
                     line_no  = LineNo,
                     contents = C},
    comp_fn(T, OrigF, [NewAcc | Acc]);
comp_fn([H | T], OrigF, Acc) ->
    io:format("Handle (2) ~p~n", [H]),
    comp_fn(T, OrigF, Acc).

comp_st([], _, _OrigF, Acc) ->
    lists:reverse(Acc);
comp_st([{call, _LNo, {atom, LNo, Fn}, Args} | T], Context, OrigF, Acc) ->
    NewAcc = make_js(fn, {LNo, Fn, Args}, OrigF, Acc),
    comp_st(T, Context, OrigF, NewAcc);
comp_st([{string, LNo, String} | T], Context, OrigF, Acc) ->
    NewAcc = make_js(string, {LNo, String}, OrigF, Acc),
    comp_st(T, Context, OrigF, NewAcc);
comp_st([{atom, LNo, Atom} | T], Context, OrigF, Acc) ->
    NewAcc = make_js(atom, {LNo, Atom}, OrigF, Acc),
    comp_st(T, Context, OrigF, NewAcc);
comp_st([{float, LNo, Float} | T], Context, OrigF, Acc) ->
    NewAcc = make_js(float, {LNo, Float}, OrigF, Acc),
    comp_st(T, Context, OrigF, NewAcc);
comp_st([{integer, LNo, Int} | T], Context, OrigF, Acc) ->
    NewAcc = make_js(int, {LNo, Int}, OrigF, Acc),
    comp_st(T, Context, OrigF, NewAcc);
comp_st([{var, LNo, Symb} | T], Context, OrigF, Acc) ->
    NewAcc = make_js(var, {LNo, Symb}, OrigF, Acc),
    comp_st(T, Context, OrigF, NewAcc);
%% comp_st([[] | T], Context, OrigF, Acc) ->
%%     NewAcc = make_js(empty_list, nonce, OrigF, Acc),
%%     comp_st(T, Context, OrigF, semi(Context, NewAcc));
comp_st([{op, LNo, Op, Lhs, Rhs} | T], Context, OrigF, Acc) ->
    NewAcc = make_js(op, {Op, LNo, Lhs, Rhs}, OrigF, Acc),
    comp_st(T, Context, OrigF, semi(Context, NewAcc));
comp_st([{match, LNo, Lhs, Rhs} | T], Context, OrigF, Acc) ->
    NewAcc = make_js(match, {LNo, Lhs, Rhs}, OrigF, Acc),
    comp_st(T, Context, OrigF, semi(Context, NewAcc));
comp_st([H | T], Context, OrigF, Acc) ->
    io:format("Handle (3) is ~p~n", [H]),
    comp_st(T, Context, OrigF, Acc).

make_js(fn, {LNo, Fn, Args}, OrigF, Acc) ->
    %% produce the pseudo-tokens in **REVERSE** order
    lists:flatten([
                   {close, ")", {OrigF, LNo}},
                   lists:reverse(comp_st([Args], expression, OrigF, [])),
                   {open, "(", {OrigF, LNo}},
                   {fn, atom_to_list(Fn), {OrigF, LNo}}
                   | Acc
                  ]);
make_js(string, {LNo, String}, OrigF, Acc) ->
    [{string, "\"" ++ String ++ "\"", {OrigF, LNo}} | Acc];
make_js(atom, {LNo, Atom}, OrigF, Acc) ->
    [{atom, "{atom: \"" ++ atom_to_list(Atom) ++ "\"}", {OrigF, LNo}} | Acc];
make_js(float, {LNo, Float}, OrigF, Acc) ->
    [{float, float_to_list(Float), {OrigF, LNo}} | Acc];
make_js(int, {LNo, Int}, OrigF, Acc) ->
    [{int, integer_to_list(Int), {OrigF, LNo}} | Acc];
make_js(var, {LNo, Symb}, OrigF, Acc) ->
    [{var, atom_to_list(Symb), {OrigF, LNo}} | Acc];
make_js(op, {Op, LNo, Lhs, Rhs}, OrigF, Acc) when Op == '+' orelse
                                                  Op == '-' orelse
                                                  Op == '*' orelse
                                                  Op == '/' ->
    %% produce the pseudo-tokens in **REVERSE** order
    lists:flatten([
                   lists:reverse(comp_st([Rhs], expression, OrigF, [])),
                   {op, atom_to_list(Op), {OrigF, LNo}},
                   lists:reverse(comp_st([Lhs], expression, OrigF, []))
                   | Acc
                  ]);
make_js(match, {LNo, Lhs, Rhs}, OrigF, Acc) ->
    %% produce the pseudo-tokens in **REVERSE** order
    lists:flatten([
                   lists:reverse(comp_st([Rhs], expression, OrigF, [])),
                   {match, "=", {OrigF, LNo}},
                   lists:reverse(comp_st([Lhs], expresssion, OrigF, []))
                   | Acc
                  ]).

semi(clause,     List) -> [{linending, ";~n", nonce} | List];
semi(expression, List) -> List.

compile_to_ast(File) ->
    IncludeDir = filename:dirname(File) ++ "/../include",
    OutDir     = filename:dirname(File),
    case compile:file(File, [
                             'P',
                             {i, IncludeDir},
                             {outdir, OutDir}
                            ]) of
        {ok, []} -> File2 = filename:rootname(File) ++ ".P",
                    io:format("File2 is ~p~n", [File2]),
                    epp:parse_file(File2, IncludeDir, []);
        error    -> io:format("Cannae compile...~n"),
                    {error, File}
    end.

collect_tokens(List) -> {ok, col2(List, 1, [], [])}.

col2([],       N,  A1, A2) -> List = lists:reverse([{N, A1} | A2]),
                              dict:from_list(List);
col2([H  | T], N2, A1, A2) -> N1 = element(2, H),
                              case N2 of
                                  N1 -> col2(T, N1, [H | A1], A2);
                                  _  -> col2([H | T], N1, [], [{N2, A1} | A2])
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

plain_log(String, File) ->
    _Return = filelib:ensure_dir(File),

    case file:open(File, [append]) of
        {ok, Id} ->
            io:fwrite(Id, "~s~n", [String]),
            file:close(Id);
        _ ->
            error
    end.
