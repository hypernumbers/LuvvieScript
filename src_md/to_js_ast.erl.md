   @author    Gordon Guthrie
   @copyright (C) 2013, Gordon Guthrie
   @doc       This module generates the javascript AST
              To see examples of the javascript AST go to
              http://esprima.org/demo/parse.html

   @end
   Created : 17 Aug 2013 by gordon@vixo.com
```erlang
    -module(to_jast).

    -export([
             make_erlang_call/3,
             make_apply/1,
             make_return/1,
             make_fail/0,
             make_declarations/1,
             make_object/1,
             make_block_statement/1,
             make_fn/3,
             make_fn_body/3,
             make_switch/2,
             make_cases/2,
             make_programme/1,
             make_identifier/2,
             make_literal/2,
             make_method/2,
             make_call_expr/2,
             make_operator/4,
             make_expression/1
            ]).

    -include_lib("core_parse.hrl").
    -include("luvviescript.hrl").
    -include("macros.hrl").

```
```
 Use this definition where you know the source map is humped
 but will need to be fixed up later
%```erlang
```erlang
    -define(TODO_SOURCEMAP, []).

    make_erlang_call('*', [A, B], Loc) ->
        {A1, B1} = rectify(A, B),
        make_operator("*", A1, B1, Loc);
    make_erlang_call('/', [A, B], Loc) ->
        {A1, B1} = rectify(A, B),
        make_operator("/", A1, B1, Loc);
    make_erlang_call('+', [A, B], Loc) ->
        {A1, B1} = rectify(A, B),
        make_operator("+", A1, B1, Loc);
    make_erlang_call('-', [A, B], Loc) ->
        {A1, B1} = rectify(A, B),
        make_operator("-", A1, B1, Loc).

    rectify(#c_var{name = A}, #c_var{name = B}) ->
        {make_identifier(atom_to_list(A), ?TODO_SOURCEMAP),
         make_identifier(atom_to_list(B), ?TODO_SOURCEMAP)}.

    make_apply(#c_apply{op = Op, args = Args}) ->
        {Name, _} = Op#c_var.name,
        make_call_expr(make_identifier(atom_to_list(Name), ?TODO_SOURCEMAP), Args).

    make_return(Return) ->
        {obj, [
               {"type",     <<"ReturnStatement">>},
               {"argument", Return}
              ]
        }.

    make_fail() ->
        make_literal("throw error", ?NOSRCMAPINFO).

    make_declarations(List) when is_list(List) ->
        Decs = make_decs(List, []),
        {obj, [
               {"type",         <<"VariableDeclaration">>},
               {"declarations", Decs},
               {"kind",         <<"var">>}
              ]
        }.

    make_decs([], Acc) ->
        lists:reverse(Acc);
    make_decs([{Name, []} | T], Acc) ->
        make_decs([{Name, null} | T], Acc);
    make_decs([{Name, Init} | T], Acc) ->
        NewAcc = {obj, [
                        {"type", <<"VariableDeclarator">>},
                        {"id",   {obj, [
                                        {"type", <<"Identifier">>},
                                        {"name", list_to_binary(Name)}
                                       ]
                                 }
                        },
                        {"init", Init}
                       ]
                 },
        make_decs(T, [NewAcc | Acc]).

    make_object({obj, List}) when is_list(List) ->
        {obj, [
               {"type",       <<"ObjectExpression">>},
               {"properties", List}
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
                    true -> Body ++ [
                                     {obj, [
                                            {"type", <<"BreakStatement">>},
                                            {"label", null}
                                           ]
                                     }
                                    ];
                    false -> Body
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
        make_expression(make_op2("=", <<"AssignmentExpression">>, Left, Right, Loc));
    make_operator("*", Left, Right, Loc) ->
        make_op2("*", <<"BinaryExpression">>, Left, Right, Loc);
    make_operator("/", Left, Right, Loc) ->
        make_op2("/", <<"BinaryExpression">>, Left, Right, Loc);
    make_operator("+", Left, Right, Loc) ->
        make_op2("+", <<"BinaryExpression">>, Left, Right, Loc);
    make_operator("-", Left, Right, Loc) ->
        make_op2("-", <<"BinaryExpression">>, Left, Right, Loc).

    make_op2(Operator, OpDesc, Left, Right, Loc) ->
        {obj,
         lists:flatten([
                        {"type",     OpDesc},
                        {"operator", enc_v(Operator)},
                        {"left",     Left},
                        {"right",    Right},
                        Loc
                       ])
        }.

    make_expression(Expr) ->
        {obj, [
               {"type",       <<"ExpressionStatement">>},
               {"expression", Expr}
              ]
        }.

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
        Got = make_switch(<<"args">>, [{0,    [J], ?WITHBREAK},
                                       {1,    [E], ?WITHBREAK},
                                       {null, [S], ?WITHOUTBREAK}]),
        %% log_output("Switch", Got, Exp),
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
        FnName   = make_identifier("simplefn", ?NOSRCMAPINFO),
        Params   = ?EMPTYJSONLIST,
        Defaults = ?EMPTYJSONLIST,
        Literal  = make_literal("banjolette", ?NOSRCMAPINFO),
        Return   = make_return(Literal),
        Body     = make_block_statement([Return]),
        FnBody   = make_fn_body(Params, Defaults, Body),
        Got      = make_fn(FnName, FnBody, ?NOSRCMAPINFO),
        %% log_output("Fns", Got, Exp),
        ?_assertEqual(Got, Exp).

    return_test_() ->
```
 var fn = function () {
 	var a;
 	var b;
 	a = 1;
 	b = 2;
 	return a/b;
 	}
```erlang
        Exp = {obj,
     [{"type",<<"ExpressionStatement">>},
      {"expression",
       {obj,
        [{"type",<<"AssignmentExpression">>},
         {"operator",<<"=">>},
         {"left",{obj,[{"type",<<"Identifier">>},{"name",<<"fn">>}]}},
         {"right",
          {obj,
           [{"type",<<"FunctionExpression">>},
            {"id",null},
            {"params",[]},
            {"defaults",[]},
            {"body",
             {obj,
              [{"type",<<"BlockStatement">>},
               {"body",
                [{obj,
                  [{"type",<<"VariableDeclaration">>},
                   {"declarations",
                    [{obj,
                      [{"type",<<"VariableDeclarator">>},
                       {"id",{obj,[{"type",<<"Identifier">>},{"name",<<"a">>}]}},
                       {"init",null}]}]},
                   {"kind",<<"var">>}]},
                 {obj,
                  [{"type",<<"VariableDeclaration">>},
                   {"declarations",
                    [{obj,
                      [{"type",<<"VariableDeclarator">>},
                       {"id",{obj,[{"type",<<"Identifier">>},{"name",<<"b">>}]}},
                       {"init",null}]}]},
                   {"kind",<<"var">>}]},
                 {obj,
                  [{"type",<<"ExpressionStatement">>},
                   {"expression",
                    {obj,
                     [{"type",<<"AssignmentExpression">>},
                      {"operator",<<"=">>},
                      {"left",{obj,[{"type",<<"Identifier">>},{"name",<<"a">>}]}},
                      {"right",
                       {obj,
                        [{"type",<<"Literal">>},
                         {"value",1},
                         {"raw",<<"1">>}]}}]}}]},
                 {obj,
                  [{"type",<<"ExpressionStatement">>},
                   {"expression",
                    {obj,
                     [{"type",<<"AssignmentExpression">>},
                      {"operator",<<"=">>},
                      {"left",{obj,[{"type",<<"Identifier">>},{"name",<<"b">>}]}},
                      {"right",
                       {obj,
                        [{"type",<<"Literal">>},
                         {"value",2},
                         {"raw",<<"2">>}]}}]}}]},
                 {obj,
                  [{"type",<<"ReturnStatement">>},
                   {"argument",
                    {obj,
                     [{"type",<<"BinaryExpression">>},
                      {"operator",<<"/">>},
                      {"left",{obj,[{"type",<<"Identifier">>},{"name",<<"a">>}]}},
                      {"right",
                       {obj,
                        [{"type",<<"Identifier">>},{"name",<<"b">>}]}}]}}]}]}]}},
            {"rest",null},
            {"generator",false},
            {"expression",false}]}}]}}]},

        FnName   = make_identifier("fn", ?NOSRCMAPINFO),
        Params   = ?EMPTYJSONLIST,
        Defaults = ?EMPTYJSONLIST,
        Decls = lists:flatten([
                               make_declarations([{"a", ?NOTINITIALISED}]),
                               make_declarations([{"b", ?NOTINITIALISED}])
                              ]),
        A1 = make_identifier("a", ?NOSRCMAPINFO),
        B1 = make_identifier("b", ?NOSRCMAPINFO),
        Ass1 = make_operator("=", A1, make_literal(1, ?NOSRCMAPINFO), ?NOSRCMAPINFO),
        Ass2 = make_operator("=", B1, make_literal(2, ?NOSRCMAPINFO), ?NOSRCMAPINFO),
        Expr    = make_operator("/", A1, B1, ?NOSRCMAPINFO),
        Return  = make_return(Expr),
        Body    = make_block_statement(lists:flatten([Decls, Ass1, Ass2, Return])),
        FnBody  = make_fn_body(Params, Defaults, Body),
        Got     = make_fn(FnName, FnBody, ?NOSRCMAPINFO),
        %% log_output("Fns", Got, Exp),
        ?_assertEqual(Got, Exp).

    declarations_test_() ->
        Exp = [{obj,[{"type",<<"VariableDeclaration">>},
                     {"declarations",
                      [{obj,[{"type",<<"VariableDeclarator">>},
                             {"id",{obj,[{"type",<<"Identifier">>},{"name",<<"a">>}]}},
                             {"init",null}]}]},
                     {"kind",<<"var">>}]},
               {obj,[{"type",<<"VariableDeclaration">>},
                     {"declarations",
                      [{obj,[{"type",<<"VariableDeclarator">>},
                             {"id",{obj,[{"type",<<"Identifier">>},{"name",<<"b">>}]}},
                             {"init",null}]}]},
                     {"kind",<<"var">>}]}],
        Got = [
               make_declarations([
                                  {"a", ?NOTINITIALISED}
                                 ]),
               make_declarations([
                                  {"b", ?NOTINITIALISED}
                                 ])
              ],
        %% log_output("Declarations", Got, Exp),
        ?_assertEqual(Got, Exp).

```
```
-endif.
