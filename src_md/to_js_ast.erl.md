   @author    Gordon Guthrie
   @copyright (C) 2013, Gordon Guthrie
   @doc       This module generates the javascript AST
              To see examples of the javascript AST go to
              http://esprima.org/demo/parse.html

   @end
   Created : 17 Aug 2013 by gordon@vixo.com
```erlang
    -module(to_js_ast).

    -export([
             make_erlang_call/3,
             make_apply/2,
             make_return/2,
             make_fail/0,
             make_declarations/2,
             make_object/2,
             make_block_statement/2,
             make_fn/3,
             make_fn_body/4,
             make_switch/3,
             make_cases/2,
             make_programme/2,
             make_identifier/2,
             make_literal/2,
             make_method/3,
             make_call_expr/3,
             make_operator/4,
             make_expression/2
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
        io:format("I rectify A is ~p B is ~p~n",[A, B]),
        {make_identifier(atom_to_list(A), ?NOSRCMAP),
         make_identifier(atom_to_list(B), ?NOSRCMAP)}.

    make_apply(#c_apply{op = Op, args = Args}, Loc) ->
        {Name, _} = Op#c_var.name,
        make_call_expr(make_identifier(atom_to_list(Name), Loc), Args, ?NOSRCMAP).

    make_return(Return, Loc) ->
        {obj, lists:flatten([
                             {"type",     <<"ReturnStatement">>},
                             {"argument", Return},
                             Loc
                            ])
        }.

    make_fail() ->
        make_literal("throw error", ?NOSRCMAP).

    make_declarations(List, Loc) when is_list(List) ->
        io:format("List is ~p~n", [List]),
        Decs = make_decs(List, []),
        {obj, lists:flatten([
                             {"type",         <<"VariableDeclaration">>},
                             {"declarations", Decs},
                             {"kind",         <<"var">>},
                             Loc
                            ])
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
                                        {"name", enc_v(Name)}
                                       ]
                                 }
                        },
                        {"init", Init}
                       ]
                 },
        make_decs(T, [NewAcc | Acc]).

    make_object({obj, List}, Loc) when is_list(List) ->
        {obj, lists:flatten([
                             {"type",       <<"ObjectExpression">>},
                             {"properties", List},
                             Loc
                            ])
        }.

    make_block_statement(Block, Loc) when is_list(Block) ->
        {obj,
         lists:flatten([
                        {"type", <<"BlockStatement">>},
                        {"body", Block},
                        Loc
                       ])
        }.

    make_fn(Left, Body, Loc) ->
        io:format("In make_fn for ~p~n", [Left]),
        _Expr = make_operator("=", Left, Body, Loc).

    make_fn_body(Params, Defaults, Body, Loc) ->
        {obj, lists:flatten([
                             {"type",       <<"FunctionExpression">>},
                             {"id",         null},
                             {"params",     Params},
                             {"defaults",   Defaults},
                             {"body",       Body},
                             {"rest",       null},
                             {"generator",  false},
                             {"expression", false},
                             Loc
                            ])
        }.

    make_switch(Variable, Cases, Loc) ->
        {obj, lists:flatten([
                             {"type",         <<"SwitchStatement">>},
                             {"discriminant", {obj, [
                                                     {"type", <<"Identifier">>},
                                       {"name", Variable}
                                                    ]
                                              }

                             },
                             {"cases", make_cases(Cases, [])},
                             Loc
                            ])
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

    make_programme(Body, Loc) when is_list(Body) ->
        {obj, lists:flatten([
                             {"type", <<"Program">>},
                             {"body", Body},
                             Loc
                            ])
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

    make_method(Obj, Fn, Loc) ->
        {obj, lists:flatten([
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
                             },
                             Loc
                            ])
        }.

    make_call_expr(Callee, Args, Loc) ->
        {obj,
         lists:flatten([
                        {"type",      <<"CallExpression">>},
                        {"callee",    Callee},
                        {"arguments", enc_v(Args)},
                        Loc
                       ])
        }.

    make_operator("=", Left, Right, Loc) ->
        Op = make_op2("=", <<"AssignmentExpression">>, Left, Right, Loc),
        make_expression(Op, ?NOSRCMAP);
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

    make_expression(Expr, Loc) ->
        {obj, lists:flatten([
                             {"type",       <<"ExpressionStatement">>},
                             {"expression", Expr},
                             Loc
                            ])
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
        code:add_patha("../deps/rfc4627_jsonrpc/ebin"),
        GotMsg = io_lib:format(Strap ++ "~n~p~n", [Got]),
        ExpMsg = io_lib:format(Strap ++ "~n~p~n", [Expected]),
        filelib:ensure_dir("/tmp/jast/junk.log"),
        GotJson = rfc4627:encode(Got),
        ExpJson = rfc4627:encode(Expected),
        GotJsonMsg = io_lib:format(Strap ++ " Json~n~s~n", [GotJson]),
        ExpJsonMsg = io_lib:format(Strap ++ " Json~n~s~n", [ExpJson]),
        make_utils:plain_log(GotMsg, "/tmp/jast/got.log"),
        make_utils:plain_log(ExpMsg, "/tmp/jast/exp.log"),
        make_utils:plain_log(GotJsonMsg, "/tmp/jast/got.log"),
        make_utils:plain_log(ExpJsonMsg, "/tmp/jast/exp.log"),
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
        J = make_expression(make_literal("jerk",  ?NOSRCMAP), ?NOSRCMAP),
        E = make_expression(make_literal("erk",   ?NOSRCMAP), ?NOSRCMAP),
        S = make_expression(make_literal("shirk", ?NOSRCMAP), ?NOSRCMAP),
        Got = make_switch(<<"args">>, [{0,    [J], ?WITHBREAK},
                                       {1,    [E], ?WITHBREAK},
                                       {null, [S], ?NOBREAK}], ?NOSRCMAP),
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
        Left   = make_identifier("_args", ?NOSRCMAP),
        Method = make_method("arguments", "length", ?NOSRCMAP),
        Right  = make_call_expr(Method, [], ?NOSRCMAP),
        Got    = make_operator("=", Left, Right, ?NOSRCMAP),
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
        FnName   = make_identifier("simplefn", ?NOSRCMAP),
        Params   = ?EMPTYJSONLIST,
        Defaults = ?EMPTYJSONLIST,
        Literal  = make_literal("banjolette", ?NOSRCMAP),
        Return   = make_return(Literal, ?NOSRCMAP),
        Body     = make_block_statement([Return], ?NOSRCMAP),
        FnBody   = make_fn_body(Params, Defaults, Body, ?NOSRCMAP),
        Got      = make_fn(FnName, FnBody, ?NOSRCMAP),
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

        FnName   = make_identifier("fn", ?NOSRCMAP),
        Params   = ?EMPTYJSONLIST,
        Defaults = ?EMPTYJSONLIST,
        Decls = lists:flatten([
                               make_declarations([{"a", ?NOTINITIALISED}], ?NOSRCMAP),
                               make_declarations([{"b", ?NOTINITIALISED}], ?NOSRCMAP)
                              ]),
        A1 = make_identifier("a", ?NOSRCMAP),
        B1 = make_identifier("b", ?NOSRCMAP),
        Ass1 = make_operator("=", A1, make_literal(1, ?NOSRCMAP), ?NOSRCMAP),
        Ass2 = make_operator("=", B1, make_literal(2, ?NOSRCMAP), ?NOSRCMAP),
        Expr    = make_operator("/", A1, B1, ?NOSRCMAP),
        Return  = make_return(Expr, ?NOSRCMAP),
        Body    = make_block_statement(lists:flatten([Decls, Ass1, Ass2, Return]),
                                       ?NOSRCMAP),
        FnBody  = make_fn_body(Params, Defaults, Body, ?NOSRCMAP),
        Got     = make_fn(FnName, FnBody, ?NOSRCMAP),
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
                                 ], ?NOSRCMAP),
               make_declarations([
                                  {"b", ?NOTINITIALISED}
                                 ], ?NOSRCMAP)
              ],
        %% log_output("Declarations", Got, Exp),
        ?_assertEqual(Got, Exp).

    fncall_test_() ->
        %% somefn = function() {
        %%	   return anotherfn();
        %% }
        Exp = {obj,
                [{"type",<<"ExpressionStatement">>},
                 {"expression",
                  {obj,
                   [{"type",<<"AssignmentExpression">>},
                    {"operator",<<"=">>},
                    {"left",{obj,[{"type",<<"Identifier">>},{"name",<<"somefn">>}]}},
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
                             [{"type",<<"ReturnStatement">>},
                              {"argument",
                               {obj,
                                [{"type",<<"CallExpression">>},
                                 {"callee",
                                  {obj,
                                   [{"type",<<"Identifier">>},{"name",<<"anotherfn">>}]}},
                                 {"arguments",[]}]}}]}]}]}},
                       {"rest",null},
                       {"generator",false},
                       {"expression",false}]}}]}}]},
        Left   = make_identifier("somefn", ?NOSRCMAP),
        Right  = make_call_expr(make_identifier("anotherfn", ?NOSRCMAP), [], ?NOSRCMAP),
        Return = make_return(Right, ?NOSRCMAP),
        Block  = make_block_statement([Return], ?NOSRCMAP),
        Body   = make_fn_body([], [], Block, ?NOSRCMAP),
        Got    = make_fn(Left, Body, ?NOSRCMAP),
        log_output("Fn Call", Got, Exp),
        ?_assertEqual(Got, Exp).

```
```
-endif.
