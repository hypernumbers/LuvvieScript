%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       This is the luvviescript compiler
%%%
%%% @end
%%% Created : 17 Aug 2013 by gordon@vixo.com
-module(luvviescript).

-export([
         canonicalise/1,
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
    % io:format("Syntax is ~p~n", [Syntax]),
    % {ok, Binary} = file:read_file(File),
    % {ok, Tokens, _} = erl_scan:string(binary_to_list(Binary), 1,
    %                                  [return_white_spaces, return_comments]),
    % io:format("Tokens is ~p~n", [Tokens]),
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
comp_st([H | T], Acc) ->
    io:format("Handle (3) is ~p~n", [H]),
    comp_st(T, Acc).

compile_to_ast(File) -> case compile:file(File, [to_pp, binary]) of
                            {ok, _, _} = Syn -> Syn;
                            error            -> {error, File}
                        end.
