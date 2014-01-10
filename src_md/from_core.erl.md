   @author    Gordon Guthrie
   @copyright (C) 2014, Gordon Guthrie
   @doc       This module prepares the (slightly amended)
              core Erlang AST for conversion tothe javascript AST

   @end
   Created : 10th January 2014 by gordon@vixo.com
```erlang
    -module(from_core).

    -export([
             conv/1
            ]).

    -include_lib("core_parse.hrl").
    -include("luvviescript.hrl").
    -include("macros.hrl").

    conv(#c_module{} = Module) ->
        #c_module{anno    = Annotations,
                  name    = Name,
                  exports = Exports,
                  attrs   = Attrs,
                  defs    = Defs} = Module,
        %% io:format("Module is called ~p~n-Annotations is ~p~n-Exports is ~p~n" ++
        %%              "-Attrs is ~p~n-Defs is ~p~n",
        %%          [Name, Annotations, Exports, Attrs, Defs]),
        Context = #js_context{name    = Name,
                              exports = Exports},
        Decl = to_jast:make_declarations([{"exports", ?EMPTYOBJECT}]),
        Body = [conv(X, Context) || X <- Defs],
        Exp  = conv_exports(Exports),
        to_jast:make_programme([Decl] ++ Exp ++ Body).

    conv({#c_var{name = {FnName, _}} = CVar, FnList}, Context) ->
        FnBody = conv_fn(FnList, ?NOSRCMAPINFO),
        Body = to_jast:make_fn_body(?EMPTYJSONLIST, ?EMPTYJSONLIST, FnBody),
        Loc = get_loc(CVar),
        Left = to_jast:make_identifier(atom_to_list(FnName), Loc),
        to_jast:make_fn(Left, Body, Loc).

    conv_exports(Exports) ->
        Exps2 = group_exps_of_diff_arity(Exports),
        [conv_exp(X) || X <- Exps2].

    conv_exp({Fn, Arities}) ->
        FnName  = to_jast:make_identifier(atom_to_list(Fn), ?NOSRCMAPINFO),
        Call    = to_jast:make_call_expr(FnName, ?NOSRCMAPINFO),
        Call2   = to_jast:make_return(Call),
        Cases   = [{X, [Call2], ?WITHBREAK} || X <- Arities] ++
            [{null, to_jast:make_return(?PATTERNMATCHFAIL), ?WITHOUTBREAK}],
        Switch  = to_jast:make_switch(<<"_args">>, Cases),
        Left    = to_jast:make_identifier("_args", ?NOSRCMAPINFO),
        Right   = to_jast:make_method("arguments", "length"),
        ArgsDef = to_jast:make_operator("=", Left, Right, ?NOSRCMAPINFO),
        Body    = to_jast:make_block_statement([
                                        ArgsDef,
                                        Switch
                                       ]),
        FnBody  = to_jast:make_fn_body(?EMPTYJSONLIST, ?EMPTYJSONLIST, Body),
        FnVar   = to_jast:make_method("exports", Fn),
        _Return = to_jast:make_fn(FnVar, FnBody, ?NOSRCMAPINFO).

    group_exps_of_diff_arity(Exports) ->
        GroupFn = fun(#c_var{name = {Fn, Arity}}, List) ->
                          NewT = case lists:keyfind(Fn, 1, List) of
                                     false -> {Fn, [Arity]};
                                     {Fn, As} -> {Fn, [Arity | As]}
                                 end,
                          lists:keystore(Fn, 1, List, NewT)
                  end,
        lists:foldl(GroupFn, [], Exports).

    conv_fn([], Acc) ->
        %% add the default case
        Cases   = lists:reverse([{null, [to_jast:make_return(?PATTERNMATCHFAIL)],
                                  ?WITHOUTBREAK} | Acc]),
        Switch  = to_jast:make_switch(<<"_args">>, Cases),
        Left    = to_jast:make_identifier("_args", ?NOSRCMAPINFO),
        Right   = to_jast:make_method("arguments", "length"),
        ArgsDef = to_jast:make_operator("=", Left, Right, ?NOSRCMAPINFO),
        _Body   = to_jast:make_block_statement([
                                        ArgsDef,
                                        Switch
                                       ]);
    conv_fn([#c_fun{vars = Vs} = CFn | T], Acc) ->
        NewAcc = {length(Vs), conv_fn2(CFn), ?WITHBREAK},
        conv_fn(T, [NewAcc | Acc]).

    conv_fn2(#c_fun{} = CFn) ->
        conv_body(CFn#c_fun.body).

    conv_body(#c_literal{val = Val} = Body) ->
        Loc = get_loc(Body),
        [to_jast:make_return(to_jast:make_literal(Val, Loc))];
    conv_body(#c_let{} = CLet) ->
        conv_let(CLet, [], []);
    conv_body(#c_apply{} = CApply) ->
        [conv_args(CApply)];
    conv_body(Body) ->
        io:format("Need to convert body ~p~n", [Body]),
        [{obj, [
                {"type", <<"not implemented conv_body (2)">>}
               ]
         }].

    conv_let([], Acc1, [H | Acc2]) ->
        Return = to_jast:make_return(H),
        lists:reverse(Acc1) ++ lists:reverse([Return | Acc2]);
    conv_let(#c_let{vars = [Var], arg = Arg, body = B} = Body, Acc1, Acc2) ->
        Loc = get_loc(Body),
        Nm = atom_to_list(Var#c_var.name),
        Decl = to_jast:make_declarations([{Nm, null}]),
        Left = to_jast:make_identifier(Nm, get_loc(Var)),
        Right = conv_args(Arg),
        Expr = to_jast:make_operator("=", Left, Right, Loc),
        case B of
            #c_let{}  -> conv_let(B, [Decl | Acc1], [Expr | Acc2]);
            #c_call{} -> NewExpr = conv_args(B),
                         conv_let([], [Decl | Acc1], [NewExpr, Expr | Acc2])
        end.

    conv_args(#c_apply{} = CApply) ->
        to_jast:make_apply(CApply);
    conv_args(#c_call{module = Mod, name = Fn, args = Args} = CCall) ->
        Loc = get_loc(CCall),
        case Mod#c_literal.val of
            erlang -> to_jast:make_erlang_call(Fn#c_literal.val, Args, Loc);
            Module -> {obj, [
                             {"type", list_to_binary("not implemented conv_args "
                                                     ++ atom_to_list(Module))}
                            ]
                      }
        end.

    get_loc(Rec) ->
        Attrs = element(2, Rec),
        case lists:keyfind("loc", 1, Attrs) of
            false -> [];
            Loc   -> [Loc]
        end.
```
