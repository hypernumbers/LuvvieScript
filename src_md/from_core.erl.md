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
        {Decl, Body} = lists:unzip([conv(X, Context) || X <- Defs]),
        NDecls = length(Decl) + 1,
        Decl2 = lists:zip([exports | Decl], lists:duplicate(NDecls, ?EMPTYOBJECT)),
        Decls = to_js_ast:make_declarations(Decl2, ?NOSRCMAP),
        Exp  = conv_exports(Exports),
        to_js_ast:make_programme([Decls] ++ Exp ++ Body, ?NOSRCMAP).

    conv({#c_var{name = {FnName, _}} = CVar, FnList}, Context) ->
        FnBody = conv_fn(FnList, ?NOSRCMAP),
        Body = to_js_ast:make_fn_body(?EMPTYJSONLIST, ?EMPTYJSONLIST, FnBody, ?NOSRCMAP),
        Loc = get_loc(CVar),
        Left = to_js_ast:make_identifier(atom_to_list(FnName), Loc),
        {FnName, to_js_ast:make_fn(Left, Body, Loc)}.

    conv_exports(Exports) ->
        Exps2 = group_exps_of_diff_arity(Exports),
        [conv_exp(X) || X <- Exps2].

    conv_exp({Fn, Arities}) ->
        FnName  = to_js_ast:make_identifier(atom_to_list(Fn), ?NOSRCMAP),
        Call    = to_js_ast:make_call_expr(FnName, [], ?NOSRCMAP),
        Call2   = to_js_ast:make_return(Call, ?NOSRCMAP),
        Cases   = [{X, [Call2], ?WITHBREAK} || X <- Arities] ++
            [{null, to_js_ast:make_return(?PATTERNMATCHFAIL, ?NOSRCMAP), ?NOBREAK}],
        Switch  = to_js_ast:make_switch(<<"_args">>, Cases, ?NOSRCMAP),
        Left    = to_js_ast:make_identifier("_args", ?NOSRCMAP),
        Right   = to_js_ast:make_method("arguments", "length", ?NOSRCMAP),
        ArgsDef = to_js_ast:make_operator("=", Left, Right, ?NOSRCMAP),
        Body    = to_js_ast:make_block_statement([
                                                ArgsDef,
                                                Switch
                                               ], ?NOSRCMAP),
        FnBody  = to_js_ast:make_fn_body(?EMPTYJSONLIST, ?EMPTYJSONLIST, Body, ?NOSRCMAP),
        FnVar   = to_js_ast:make_method("exports", Fn, ?NOSRCMAP),
        _Return = to_js_ast:make_fn(FnVar, FnBody, ?NOSRCMAP).

    group_exps_of_diff_arity(Exports) ->
        GroupFn = fun(#c_var{name = {Fn, Arity}}, List) ->
                          NewT = case lists:keyfind(Fn, 1, List) of
                                     false    -> {Fn, [Arity]};
                                     {Fn, As} -> {Fn, [Arity | As]}
                                 end,
                          lists:keystore(Fn, 1, List, NewT)
                  end,
        lists:foldl(GroupFn, [], Exports).

    conv_fn([], Acc) ->
        %% add the default case
        Cases   = lists:reverse([{null, [to_js_ast:make_return(?PATTERNMATCHFAIL,
                                                             ?NOSRCMAP)],
                                  ?NOBREAK} | Acc]),
        Switch  = to_js_ast:make_switch(<<"_args">>, Cases, ?NOSRCMAP),
        Left    = to_js_ast:make_identifier("_args", ?NOSRCMAP),
        Right   = to_js_ast:make_method("arguments", "length", ?NOSRCMAP),
        ArgsDef = to_js_ast:make_operator("=", Left, Right, ?NOSRCMAP),
        _Body   = to_js_ast:make_block_statement([
                                        ArgsDef,
                                        Switch
                                       ], ?NOSRCMAP);
    conv_fn([#c_fun{vars = Vs} = CFn | T], Acc) ->
        NewAcc = {length(Vs), conv_fn2(CFn), ?WITHBREAK},
        conv_fn(T, [NewAcc | Acc]).

    conv_fn2(#c_fun{} = CFn) ->
        conv_body(CFn#c_fun.body).

    conv_body(#c_literal{val = Val} = Body) ->
        Loc = get_loc(Body),
        [to_js_ast:make_return(to_js_ast:make_literal(Val, Loc), ?NOSRCMAP)];
    conv_body(#c_let{} = CLet) ->
        conv_let(CLet, [], []);
    conv_body(#c_apply{} = CApply) ->
        [to_js_ast:make_return(conv_args(CApply), ?NOSRCMAP)];
    conv_body(Body) ->
        io:format("Need to convert body ~p~n", [Body]),
        [{obj, [
                {"type", <<"not implemented conv_body (2)">>}
               ]
         }].

    conv_let([], Acc1, [H | Acc2]) ->
        Return = to_js_ast:make_return(H, ?NOSRCMAP),
        lists:reverse(Acc1) ++ lists:reverse([Return | Acc2]);
    conv_let(#c_let{vars = [Var], arg = Arg, body = B} = Body, Acc1, Acc2) ->
        Loc = get_loc(Body),
        Nm = atom_to_list(Var#c_var.name),
        Decl = to_js_ast:make_declarations([{Nm, null}], ?NOSRCMAP),
        Left = to_js_ast:make_identifier(Nm, get_loc(Var)),
        Right = conv_args(Arg),
        Expr = to_js_ast:make_operator("=", Left, Right, Loc),
        case B of
            #c_let{}  -> conv_let(B, [Decl | Acc1], [Expr | Acc2]);
            #c_call{} -> NewExpr = conv_args(B),
                         conv_let([], [Decl | Acc1], [NewExpr, Expr | Acc2])
        end.

    conv_args(#c_apply{} = CApply) ->
        to_js_ast:make_apply(CApply, ?NOSRCMAP);
    conv_args(#c_call{module = Mod, name = Fn, args = Args} = CCall) ->
        Loc = get_loc(CCall),
        case Mod#c_literal.val of
            erlang -> to_js_ast:make_erlang_call(Fn#c_literal.val, Args, Loc);
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
