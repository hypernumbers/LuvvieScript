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

    -define(WITHBREAK,    true).
    -define(WITHOUTBREAK, false).

    conv(#c_module{} = Module) ->
        #c_module{anno    = Annotations,
                  name    = Name,
                  exports = Exports,
                  attrs   = Attrs,
                  defs    = Defs} = Module,
        io:format("Module is called ~p~n-Annotations is ~p~n-Exports is ~p~n" ++
                      "-Attrs is ~p~n-Defs is ~p~n",
                  [Name, Annotations, Exports, Attrs, Defs]),
        Context = #js_context{name    = Name,
                               exports = Exports},
        Body = [conv(X, Context) || X <- Defs],
        io:format("Body is ~p~n", [Body]),
        ok.

    conv({#c_var{name = {FnName, _}} = CVar, FnList}, Context) ->
        io:format("Convert ~p ~p~n-using ~p~n", [FnName, FnList, Context]),
        FnBody = conv_fn(FnList, []),
        Body = make_fn_body([], [], FnBody),
        Loc = get_loc(CVar),
        make_fn(FnName, Body, Loc).

    conv_fn([], Acc) ->
        %% add the default case
        Cases   = lists:reverse([{null, "throw error", ?WITHOUTBREAK} | Acc]),
        Switch  = make_switch(<<"_args">>, Cases),
        Left    = make_literal("_args", []),
        Method  = make_method("arguments", "length"),
        Right   = make_call_expr(Method, []),
        ArgsDef = make_operator("=", Left, Right, []),
        _Body   = make_block_statement([
                                     ArgsDef,
                                     Switch
                                    ]);
    conv_fn([#c_fun{vars = Vs} = CFn | T], Acc) ->
        NewAcc = {length(Vs), conv_fn2(CFn), ?WITHBREAK},
        conv_fn(T, [NewAcc | Acc]).

    conv_fn2(#c_fun{} = CFn) ->
        io:format("CFn is ~p~n", [CFn]),
        Loc = get_loc(CFn),
        conv_body(CFn#c_fun.body, Loc).

    conv_body(Body, Loc) ->
        io:format("Convert body ~p~n", [Body]),
        xxx_make_body.

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

    make_fn(FNName, Body, Loc) ->
        Left = make_identifier(FNName, Loc),
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
    raw_enc_v(Atom) when is_atom(Atom)   -> Atom;
    raw_enc_v(Int)  when is_integer(Int) -> list_to_binary(integer_to_list(Int));
    raw_enc_v(Flt)  when is_float(Flt)   ->
        %% definetaly a better way to test this (3.0 = "3")
        Str = case erlang:trunc(Flt) == Flt andalso Flt < 99999 of
                  true  -> integer_to_list(erlang:trunc(Flt));
                  false -> string:to_upper(mochinum:digits(Flt))
              end,
        list_to_binary(Str).

    enc_v(Str)  when is_list(Str)    -> list_to_binary(Str);
    enc_v(Atom) when is_atom(Atom)   -> Atom;
    enc_v(Int)  when is_integer(Int) -> Int;
    enc_v(Flt)  when is_float(Flt)   -> Flt.

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
        J = make_expression(make_literal("jerk",  [])),
        E = make_expression(make_literal("erk",   [])),
        S = make_expression(make_literal("shirk", [])),
        Got = make_switch(<<"args">>, [{0,    J, ?WITHBREAK},
                                       {1,    E, ?WITHBREAK},
                                       {null, S, ?WITHOUTBREAK}]),
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
                                  {"arguments",<<>>}
                                 ]
                            }
                           }
                          ]
                     }
                    }
                   ]
              },
        Left   = make_identifier("_args", []),
        Method = make_method("arguments", "length"),
        Right  = make_call_expr(Method, []),
        Got    = make_operator("=", Left, Right, []),
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
        Params   = [],
        Defaults = [],
        Literal  = make_literal("banjolette", []),
        Return   = make_return(Literal),
        Body     = make_block_statement([Return]),
        FnBody   = make_fn_body(Params, Defaults, Body),
        Got      = make_fn(FnName, FnBody, []),
        log("Literal", Literal),
        log("Return",  Return),
        log("Body",    Body),
        log("FnBody",  FnBody),
        log("Got",     Got),
        log_output("Fns", Got, Exp),
        ?_assertEqual(Got, Exp).

    log(Prefix, Term) ->
        filelib:ensure_dir("/tmp/jast/junk.log"),
        Msg = io_lib:format(Prefix ++ "~n~p", [Term]),
        make_utils:plain_log(Msg, "/tmp/jast/debug.log").

```
```
-endif.
