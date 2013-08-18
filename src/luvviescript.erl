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
    comp2(Syntax, #module{name = Name}, Tokens2, []).

comp2([], Module, _Tokens, Acc) ->
    Module#module{contents = lists:reverse(Acc)};
comp2([{function, LineNo, Fn, Arity, Contents} | T], Module, Tokens, Acc) ->
    C = comp_fn(Contents, Tokens, []),
    NewAcc = #function{name     = Fn,
                       arity    = Arity,
                       line_no  = LineNo,
                       contents = C},
    comp2(T, Module, Tokens, [NewAcc | Acc]);
comp2([{eof, _} | T], Module, Tokens, Acc) ->
    comp2(T, Module, Tokens, Acc);
comp2([{attribute, _, _, _} = A | T], Module, Tokens, Acc) ->
    #module{attributes = Attrs} = Module,
    NewM = Module#module{attributes = [A | Attrs]},
    comp2(T, NewM, Tokens, Acc);
comp2([H | T], Module, Tokens, Acc) ->
    io:format("Handle ~p~n", [H]),
    comp2(T, Module, Tokens, Acc).

comp_fn([], _Tokens, Acc) ->
    lists:reverse(Acc);
comp_fn([{clause, LineNo, Params, Guards, Contents} | T], Tokens, Acc) ->
    C = comp_st(Contents, []),
    NewAcc = #clause{params   = Params,
                     guards   = Guards,
                     line_no  = LineNo,
                     contents = C},
    comp_fn(T, Tokens, [NewAcc | Acc]);
comp_fn([H | T], Tokens, Acc) ->
    io:format("Handle (2) ~p~n", [H]),
    comp_fn(T, Tokens, Acc).

comp_st([], Acc) ->
    lists:reverse(Acc);
comp_st([H | T], Acc) ->
    io:format("Handle (3) is ~p~n", [H]),
    comp_st(T, Acc).

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
