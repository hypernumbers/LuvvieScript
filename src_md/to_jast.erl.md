   @author    Gordon Guthrie
   @copyright (C) 2013, Gordon Guthrie
   @doc       This module converts the (slightly amended)
              core Erlang AST to the javascript one
              to see examples of the javascript AST go to
              http://esprima.org/demo/parse.html

   @end
   Created : 17 Aug 2013 by gordon@vixo.com
```erlang
    -module(to_jast).

    -export([
             conv/1
            ]).

    -include_lib("core_parse.hrl").
    -include("luvviescript.hrl").

    -define(WITHBREAK,        true).
    -define(WITHOUTBREAK,     false).
    -define(NOSRCMAPINFO,     []).
    -define(PATTERNMATCHFAIL, make_fail()).
    -define(EMPTYJSONLIST,    []).
    -define(NOTINITIALISED,   []).
    -define(EMPTYOBJECT,      make_object({obj, []})).

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
        Decl = make_declarations([{<<"exports">>, ?EMPTYOBJECT}]),
        Body = [conv(X, Context) || X <- Defs],
        Exp  = conv_exports(Exports),

        make_programme([Decl] ++ Exp ++ Body).

    conv({#c_var{name = {FnName, _}} = CVar, FnList}, Context) ->
        FnBody = conv_fn(FnList, ?NOSRCMAPINFO),
        Body = make_fn_body(?EMPTYJSONLIST, ?EMPTYJSONLIST, FnBody),
        Loc = get_loc(CVar),
        Left = make_identifier(atom_to_list(FnName), Loc),
        make_fn(Left, Body, Loc).

    conv_exports(Exports) ->
        Exps2 = group_exps_of_diff_arity(Exports),
        [conv_exp(X) || X <- Exps2].

    conv_exp({Fn, Arities}) ->
        FnName  = make_identifier(atom_to_list(Fn), ?NOSRCMAPINFO),
        Call    = make_call_expr(FnName, ?NOSRCMAPINFO),
        Cases   = [{X, make_expression(Call), ?WITHBREAK} || X <- Arities] ++
            [{null, ?PATTERNMATCHFAIL, ?WITHOUTBREAK}],
        Switch  = make_switch(<<"_args">>, Cases),
        Left    = make_identifier("_args", ?NOSRCMAPINFO),
        Method  = make_method("arguments", "length"),
        Right   = make_call_expr(Method, ?NOSRCMAPINFO),
        ArgsDef = make_operator("=", Left, Right, ?NOSRCMAPINFO),
        Body    = make_block_statement([
                                        ArgsDef,
                                        Switch
                                       ]),
        FnBody  = make_fn_body(?EMPTYJSONLIST, ?EMPTYJSONLIST, Body),
        FnVar   = make_method("exports", Fn),
        _Return = make_fn(FnVar, FnBody, ?NOSRCMAPINFO).

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
        Cases   = lists:reverse([{null, ?PATTERNMATCHFAIL, ?WITHOUTBREAK} | Acc]),
        Switch  = make_switch(<<"_args">>, Cases),
        Left    = make_identifier("_args", ?NOSRCMAPINFO),
        Method  = make_method("arguments", "length"),
        Right   = make_call_expr(Method, ?NOSRCMAPINFO),
        ArgsDef = make_operator("=", Left, Right, ?NOSRCMAPINFO),
        _Body   = make_block_statement([
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
        make_expression(make_literal(Val, Loc));
    conv_body(Body) ->
        io:format("Convert body ~p~n", [Body]),
        xxx_make_body.

    make_fail() ->
        make_expression(make_literal("throw error", ?NOSRCMAPINFO)).

    make_declarations(List) when is_list(List) ->
        Decs = make_decs(List, []),
        {obj, [
               {"type",         <<"VariableDeclaration">>},
               {"declarations", Decs},
               {"kind",         <<"var">>}
               ]
        }.

    make_object({obj, List}) when is_list(List) ->
        {obj, [
               {"type",       <<"ObjectExpression">>},
               {"properties", List}
               ]
        }.

    make_decs([], Acc) ->
        lists:reverse(Acc);
    make_decs([{Name, []} | T], Acc) when is_binary(Name) ->
        make_decs([{Name, null} | T], Acc);
    make_decs([{Name, Init} | T], Acc) when is_binary(Name) ->
        NewAcc = {obj, [
                        {"type", <<"VariableDeclarator">>},
                        {"id",   {obj, [
                                        {"type", <<"Identifier">>},
                                        {"name", Name}
                                       ]
                                 }
                        },
                        {"init", Init}
                       ]
                 },
        make_decs(T, [NewAcc | Acc]).

    make_return(Ret) ->
        {obj,
         [
          {"type",     <<"ReturnStatement">>},
          {"argument", Ret}
         ]
        }.

    make_block_statement(Block) ->
        {obj,
         [
          {"type", <<"BlockStatement">>},
          {"body", Block}
         ]
        }.

    make_fn(Left, Body, Loc) ->
        _Expr = make_operator("=", Left, Body, Loc).

    make_fn_body(Params, Defaults, Body) ->
        {obj, [
               {"type",       <<"FunctionExpression">>},
               {"id",         null},
               {"params",     Params},
               {"defaults",   Defaults},
               {"body",       Body},
               {"rest",       null},
               {"generator",  false},
               {"expression", false}
              ]
        }.

    make_switch(Variable, Cases) ->
        {obj, [
               {"type",         <<"SwitchStatement">>},
               {"discriminant", {obj, [
                                       {"type", <<"Identifier">>},
                                       {"name", Variable}
                                      ]
                                }

               },
               {"cases", make_cases(Cases, [])}
              ]
        }.

    make_cases([], Acc) ->
        lists:reverse(Acc);
    make_cases([{Val, Body, HasBreak} | T], Acc) ->
        Body2 = case HasBreak of
                    true -> [Body] ++ [
                                       {obj, [
                                              {"type", <<"BreakStatement">>},
                                              {"label", null}
                                             ]
                                       }
                                      ];
                    false -> [Body]
                end,
        NewAcc = {obj, [
                        {"type",       <<"SwitchCase">>},
                        {"test",       make_literal(Val, [])},
                        {"consequent", Body2}
                       ]
                 },
        make_cases(T, [NewAcc | Acc]).

    make_programme(Body) when is_list(Body) ->
        {obj, [
               {"type", <<"Program">>},
               {"body", Body}
              ]
        }.

    make_identifier(Val, Loc) ->
        {obj, lists:flatten([
                             {"type", <<"Identifier">>},
                             {"name", enc_v(Val)},
                             Loc
                            ])
        }.

    make_literal(null, _Loc) ->
        null;
    make_literal(Val, Loc) ->
        {obj, lists:flatten([
                             {"type",  <<"Literal">>},
                             {"value", enc_v(Val)},
                             {"raw",   raw_enc_v(Val)},
                             Loc
                            ])
        }.

    make_method(Obj, Fn) ->
        {obj, [
               {"type",     <<"MemberExpression">>},
               {"computed", false},
               {"object",   {obj, [
                                   {"type", <<"Identifier">>},
                                   {"name", enc_v(Obj)}
                                ]
                          }
               },
               {"property", {obj,
                             [
                              {"type", <<"Identifier">>},
                              {"name", enc_v(Fn)}
                              ]
                             }
                }
              ]
        }.

    make_call_expr(Callee, Args) ->
        {obj,
         [
          {"type",      <<"CallExpression">>},
          {"callee",    Callee},
          {"arguments", enc_v(Args)}
         ]
        }.

    make_operator("=", Left, Right, Loc) ->
        make_op2("=", <<"AssignmentExpression">>, Left, Right, Loc).

    make_op2(Operator, OpDesc, Left, Right, Loc) ->
        {obj, [
               {"type",       <<"ExpressionStatement">>},
               {"expression", {obj,
                               lists:flatten([
                                              {"type",     OpDesc},
                                              {"operator", enc_v(Operator)},
                                              {"left",     Left},
                                              {"right",    Right},
                                              Loc
                                             ])
                              }
               }
              ]
        }.

    make_expression(Expr) ->
        {obj, [
               {"type",       <<"ExpressionStatement">>},
               {"expression", Expr}
              ]
        }.

```
```
 conv({integer, {Line, {ColStart, ColEnd}}, Int}, _Context) ->
     {
      {"type", "Literal"},
      {"value", Int},
      {"raw",   integer_to_list(Int)},
      loc(Line, ColStart, ColEnd)
     };
 conv({float, {Line, {ColStart, ColEnd}}, Float}, _Context) ->
     {
      {"type", "Literal"},
      {"value", Float},
      {"raw",   float_to_list(Float)},
      loc(Line, ColStart, ColEnd)
     };
 conv({string, {Line, {ColStart, ColEnd}}, String}, _Context) ->
     {
      {"type", "Literal"},
      {"value", String},
      {"raw",   "\"" ++ String ++ "\""},
      loc(Line, ColStart, ColEnd)
     };
 conv({atom, {Line, {ColStart, ColEnd}}, Boolean}, _Context)
     when Boolean =:= true  orelse
          Boolean =:= false ->
     {
      {"type", "Literal"},
      {"value", Boolean},
      {"raw",   atom_to_list(Boolean)},
      loc(Line, ColStart, ColEnd)
     };
 conv({atom, {Line, {ColStart, ColEnd}}, Atom}, _Context) ->
     {
       {"type", "Property"},
       {"key", {
          {"type", "Identifier"},
          {"name", "atom"}
         }
       },
       {"value", {
          {"type",  "Literal"},
          {"value", atom_to_list(Atom)},
          {"raw",   atom_to_list(Atom)},
           loc(Line, ColStart, ColEnd)
         }
       }
     };
 %% TODO understand the types of expression (eg ExpressionStatement)
 conv({match, {_Line, none}, Left, Right}, _Context) ->
     {
       {"type",     "AssignmentExpression"},
       {"operator", "="},
       {"left",     Left},
       {"right",    Right}
     };
 conv({singleton_fn, Fn}, _Context) ->
     Fn;

```erlang
    get_loc(Rec) ->
        Attrs = element(2, Rec),
        case lists:keyfind("loc", 1, Attrs) of
            false -> [];
            Loc   -> [Loc]
        end.

    loc(Line, Start, End) ->
        {"loc", [{"start", [{"line", Line}, {"column", Start}]},
                 {"end",   [{"line", Line}, {"column", End}]}]}.

    raw_enc_v(Str)  when is_list(Str)    -> enc_v("\"" ++ Str ++ "\"");
    raw_enc_v(Atom) when is_atom(Atom)   -> Atom;  %% Todo fix up
    raw_enc_v(Int)  when is_integer(Int) -> list_to_binary(integer_to_list(Int));
    raw_enc_v(Flt)  when is_float(Flt)   ->
        %% definetaly a better way to test this (3.0 = "3")
        Str = case erlang:trunc(Flt) == Flt andalso Flt < 99999 of
                  true  -> integer_to_list(erlang:trunc(Flt));
                  false -> string:to_upper(mochinum:digits(Flt))
              end,
        list_to_binary(Str).

    enc_v([])                         -> [];
    enc_v(Str)   when is_list(Str)    -> list_to_binary(Str);
    enc_v(Atom)  when is_atom(Atom)   -> Atom;
    enc_v(Int)   when is_integer(Int) -> Int;
    enc_v(Flt)   when is_float(Flt)   -> Flt;
    enc_v(Tuple) when is_tuple(Tuple) -> Tuple.

```
```
 ```

 Unit Tests

```erlang

```
```
-ifdef(TEST).
```erlang
    -include_lib("eunit/include/eunit.hrl").

    log_output(Strap, Got, Expected) ->
        GotMsg = io_lib:format(Strap ++ "~n~p~n", [Got]),
        ExpMsg = io_lib:format(Strap ++ "~n~p~n", [Expected]),
        filelib:ensure_dir("/tmp/jast/junk.log"),
        make_utils:plain_log(GotMsg, "/tmp/jast/got.log"),
        make_utils:plain_log(ExpMsg, "/tmp/jast/exp.log"),
        ok.

    log(Prefix, Term) ->
        filelib:ensure_dir("/tmp/jast/junk.log"),
        Msg = io_lib:format(Prefix ++ "~n~p", [Term]),
        make_utils:plain_log(Msg, "/tmp/jast/debug.log").

    switch_test_() ->
        Exp = {obj,[{"type",<<"SwitchStatement">>},
          {"discriminant",{obj,[{"type",<<"Identifier">>},{"name",<<"args">>}]}},
          {"cases",
           [{obj,[{"type",<<"SwitchCase">>},
                  {"test",
                   {obj,[{"type",<<"Literal">>},{"value",0},{"raw",<<"0">>}]}},
                  {"consequent",
                   [{obj,[{"type",<<"ExpressionStatement">>},
                          {"expression",
                           {obj,[{"type",<<"Literal">>},
                                 {"value",<<"jerk">>},
                                 {"raw",<<"\"jerk\"">>}]}}]},
                    {obj,[{"type",<<"BreakStatement">>},{"label",null}]}]}]},
            {obj,[{"type",<<"SwitchCase">>},
                  {"test",
                   {obj,[{"type",<<"Literal">>},{"value",1},{"raw",<<"1">>}]}},
                  {"consequent",
                   [{obj,[{"type",<<"ExpressionStatement">>},
                          {"expression",
                           {obj,[{"type",<<"Literal">>},
                                 {"value",<<"erk">>},
                                 {"raw",<<"\"erk\"">>}]}}]},
                    {obj,[{"type",<<"BreakStatement">>},{"label",null}]}]}]},
            {obj,[{"type",<<"SwitchCase">>},
                  {"test",null},
                  {"consequent",
                   [{obj,[{"type",<<"ExpressionStatement">>},
                          {"expression",
                           {obj,[{"type",<<"Literal">>},
                                 {"value",<<"shirk">>},
                                 {"raw",<<"\"shirk\"">>}]}}]}]}]}]}]},
        J = make_expression(make_literal("jerk",  ?NOSRCMAPINFO)),
        E = make_expression(make_literal("erk",   ?NOSRCMAPINFO)),
        S = make_expression(make_literal("shirk", ?NOSRCMAPINFO)),
        Got = make_switch(<<"args">>, [{0,    J, ?WITHBREAK},
                                       {1,    E, ?WITHBREAK},
                                       {null, S, ?WITHOUTBREAK}]),
        log_output("Switch", Got, Exp),
        ?_assertEqual(Got, Exp).

    args_test_() ->
        Exp = {obj,[
                    {"type",<<"ExpressionStatement">>},
                    {"expression",
                     {obj,[
                           {"type",<<"AssignmentExpression">>},
                           {"operator",<<"=">>},
                           {"left",
                            {obj,[
                                  {"type",<<"Identifier">>},
                                  {"name",<<"_args">>}
                                 ]
                            }
                           },
                           {"right",
                            {obj,[
                                  {"type",<<"CallExpression">>},
                                  {"callee",
                                   {obj,[
                                         {"type",<<"MemberExpression">>},
                                         {"computed",false},
                                         {"object",
                                          {obj,[
                                                {"type",<<"Identifier">>},
                                                {"name",<<"arguments">>}
                                               ]
                                          }
                                         },
                                         {"property",
                                          {obj,[
                                                {"type",<<"Identifier">>},
                                                {"name",<<"length">>}
                                               ]
                                          }
                                         }
                                        ]
                                   }
                                  },
                                  {"arguments", []}
                                 ]
                            }
                           }
                          ]
                     }
                    }
                   ]
              },
        Left   = make_identifier("_args", ?NOSRCMAPINFO),
        Method = make_method("arguments", "length"),
        Right  = make_call_expr(Method, ?NOSRCMAPINFO),
        Got    = make_operator("=", Left, Right, ?NOSRCMAPINFO),
        %% log_output("Args", Got, Exp),
        ?_assertEqual(Got, Exp).

    fns_test_() ->
        Exp = {obj,
               [
                {"type",<<"ExpressionStatement">>},
                {"expression",
                 {obj,
                  [
                   {"type",<<"AssignmentExpression">>},
                   {"operator",<<"=">>},
                   {"left",
                    {obj,
                     [
                      {"type",<<"Identifier">>},
                      {"name",<<"simplefn">>}
                     ]}},
                   {"right",
                    {obj,
                     [
                      {"type",<<"FunctionExpression">>},
                      {"id",null},
                      {"params",[]},
                      {"defaults",[]},
                      {"body",
                       {obj,
                        [
                         {"type",<<"BlockStatement">>},
                         {"body",
                          [
                           {obj,
                            [
                             {"type",<<"ReturnStatement">>},
                             {"argument",
                              {obj,
                               [{"type",<<"Literal">>},
                                {"value",<<"banjolette">>},
                                {"raw",<<"\"banjolette\"">>}
                               ]}}
                            ]}
                          ]}
                        ]}},
                      {"rest",null},
                      {"generator",false},
                      {"expression",false}
                     ]}}
                  ]}}
               ]},
        FnName   = "simplefn",
        Params   = ?EMPTYJSONLIST,
        Defaults = ?EMPTYJSONLIST,
        Literal  = make_literal("banjolette", ?NOSRCMAPINFO),
        Return   = make_return(Literal),
        Body     = make_block_statement([Return]),
        FnBody   = make_fn_body(Params, Defaults, Body),
        Got      = make_fn(FnName, FnBody, ?NOSRCMAPINFO),
        log("Literal", Literal),
        log("Return",  Return),
        log("Body",    Body),
        log("FnBody",  FnBody),
        log("Got",     Got),
        %% log_output("Fns", Got, Exp),
        ?_assertEqual(Got, Exp).

```
```
-endif.
