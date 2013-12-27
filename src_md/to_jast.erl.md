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

    conv({#c_var{} = FnName, FnList}, Context) ->
        io:format("Convert ~p ~p~n", [FnName, FnList]),
        FnBody = make_fn(FnList, []),
        io:format("FnBody is ~p~n", [FnBody]),
        skipping.

    make_fn([], Acc) ->
        %% add the default case
        Cases = lists:reverse([{null, "throw error", ?WITHOUTBREAK} | Acc]),
        Switch = make_switch(<<"_args">>, Cases),
        [
         define_args,
         Switch
        ];
    make_fn([#c_fun{vars = Vs} = CFn | T], Acc) ->
        NewAcc = {length(Vs), make_fn2(CFn), ?WITHBREAK},
        make_fn(T, [NewAcc | Acc]).

    make_fn2(#c_fun{}) ->
        make_function_body.

    make_switch(Variable, Cases) ->
        {obj, [
               {"type", <<"SwitchStatement">>},
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
                        {"type", <<"SwitchCase">>},
                        {"test", make_literal(Val)},
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

    make_literal(null) ->
        null;
    make_literal(Val) ->
        {obj, [
               {"type", <<"Literal">>},
               {"value", enc_v(Val)},
               {"raw", raw_enc_v(Val)}
              ]
        }.

    make_expression(Expr) ->
        {obj, [
               {"type", <<"ExpressionStatement">>},
               {"expression", Expr}
              ]
        }.

```
make_fn(Name,

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

    basic_test_() ->
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
        J = make_expression(make_literal("jerk")),
        E = make_expression(make_literal("erk")),
        S = make_expression(make_literal("shirk")),
        Got = make_switch(<<"args">>, [{0,    J, ?WITHBREAK},
                                       {1,    E, ?WITHBREAK},
                                       {null, S, ?WITHOUTBREAK}]),
        Msg = io_lib:format("Exp is ~p~nGot is ~p~n", [Exp, Got]),
        make_utils:plain_log(Msg, "/tmp/to_jast.log"),
        ?_assertEqual(Got, Exp).

```
```
-endif.