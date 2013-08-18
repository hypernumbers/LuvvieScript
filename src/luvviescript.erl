%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       This is the luvviescript compiler
%%%
%%% @end
%%% Created : 17 Aug 2013 by gordon@vixo.com
-module(luvviescript).

-export([
         compile/1
        ]).

-record(module, {
          name,
          attributes,
          process_dictionary,
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
    {ok, _, Syntax} = compile_to_ast(File),
    {ok, Binary}    = file:read_file(File),
    {ok, Tokens, _} = erl_scan:string(binary_to_list(Binary), 1,
                                      [return_white_spaces, return_comments]),
    {ok, Tokens2} = collect_tokens(Tokens),
    Name = filename:rootname(filename:basename(File)),
    comp2(Syntax, #module{name = Name}, []).

comp2([], Module, Acc) ->
    Module#module{contents = lists:reverse(Acc)};
comp2([{function, LineNo, Fn, Arity, Contents} | T], Module, Acc) ->
    C = comp_fn(Contents, []),
    NewAcc = #function{name     = Fn,
                       arity    = Arity,
                       line_no  = LineNo,
                       contents = C},
    comp2(T, Module, [NewAcc | Acc]);
comp2([{eof, _} | T], Module, Acc) ->
    comp2(T, Module, Acc);
comp2([{attribute, _, _, _} = A | T], Module, Acc) ->
    #module{attributes = Attrs} = Module,
    NewM = Module#module{attributes = [A | Attrs]},
    comp2(T, NewM, Acc);
comp2([H | T], Module, Acc) ->
    io:format("Handle ~p~n", [H]),
    comp2(T, Module, Acc).

comp_fn([], Acc) ->
    lists:reverse(Acc);
comp_fn([{clause, LineNo, Params, Guards, Contents} | T], Acc) ->
    C = comp_st(Contents, []),
    NewAcc = #clause{params   = Params,
                     guards   = Guards,
                     line_no  = LineNo,
                     contents = C},
    comp_fn(T, [NewAcc | Acc]);
comp_fn([H | T], Acc) ->
    io:format("Handle (2) ~p~n", [H]),
    comp_fn(T, Acc).

comp_st([], Acc) ->
    lists:reverse(Acc);
comp_st([{atom, LineNo, Int} | T], Acc) ->
    NewAcc = make_js(atom, {LineNo, atom}, Acc),
    comp_st(T, NewAcc);
comp_st([{float, LineNo, float} | T], Acc) ->
    NewAcc = make_js(float, {LineNo, float}, Acc),
    comp_st(T, NewAcc);
comp_st([{integer, LineNo, Int} | T], Acc) ->
    NewAcc = make_js(int, {LineNo, Int}, Acc),
    comp_st(T, NewAcc);
comp_st([{var, LineNo, Symb} | T], Acc) ->
    NewAcc = make_js(var, {LineNo, Symb}, Acc),
    comp_st(T, NewAcc);
comp_st([{op, LineNo, Op, Lhs, Rhs} | T], Acc) ->
    NewAcc = make_js(op, {Op, LineNo, Lhs, Rhs}, Acc),
    comp_st(T, NewAcc);
comp_st([{match, LineNo, Lhs, Rhs} | T], Acc) ->
    NewAcc = make_js(match, {LineNo, Lhs, Rhs}, Acc),
    comp_st(T, NewAcc);
comp_st([H | T], Acc) ->
    io:format("Handle (3) is ~p~n", [H]),
    comp_st(T, Acc).

make_js(atom, {LineNo, Atom}, Acc) ->
    [
     wsp(),
     {atom, "{atom: " ++ atom_to_list(Atom) ++ "}", LineNo}
     | Acc
    ];
make_js(float, {LineNo, Float}, Acc) ->
    [
     wsp(),
     {float, float_to_list(Float), LineNo}
     | Acc
    ];
make_js(int, {LineNo, Int}, Acc) ->
    [
     wsp(),
     {int, integer_to_list(Int), LineNo}
     | Acc
    ];
make_js(var, {LineNo, Symb}, Acc) ->
    [
     wsp(),
     {var, atom_to_list(Symb), LineNo}
     | Acc
    ];
make_js(op, {Op, LineNo, Lhs, Rhs}, Acc) when Op == '+' orelse
                                              Op == '-' orelse
                                              Op == '*' ->
    %% produce the pseudo-tokens in **REVERSE** order
    lists:flatten([
                   comp_st([Rhs], []),
                   wsp(),
                   {op, atom_to_list(Op), LineNo},
                   comp_st([Lhs], [])
                   | Acc
                  ]);
make_js(op, {Op, LineNo, Lhs, Rhs}, Acc) when Op == '/' ->
    %% produce the pseudo-tokens in **REVERSE** order
    lists:flatten([
                   comp_st([Rhs], []),
                   {op, atom_to_list(Op), LineNo},
                   comp_st([Lhs], [])
                   | Acc
                  ]);
make_js(match, {LineNo, Lhs, Rhs}, Acc) ->
    %% produce the pseudo-tokens in **REVERSE** order
    lists:flatten([
                   comp_st([Rhs], []),
                   wsp(),
                   {match, "=", LineNo},
                   comp_st([Lhs], [])
                   | Acc
                  ]).

wsp() -> {whitespace, " "}.

compile_to_ast(File) -> case compile:file(File, [to_pp, binary]) of
                            {ok, _, _} = Syn -> Syn;
                            error            -> {error, File}
                        end.

collect_tokens(List) -> {ok, col2(List, 1, [], [])}.

col2([],       N,  A1, A2) -> List = lists:reverse([{N, A1} | A2]),
                              dict:from_list(List);
col2([H  | T], N2, A1, A2) -> N1 = element(2, H),
                              case N2 of
                                  N1 -> col2(T, N1, [H | A1], A2);
                                  _  -> col2([H | T], N1, [], [{N2, A1} | A2])
                              end.
