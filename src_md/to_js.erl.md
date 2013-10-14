   @author    Gordon Guthrie
   @copyright (C) 2013, Gordon Guthrie
   @doc       This module converts the (slightly amended)
              Erlang AST to the javascript one
              to see examples of the javascript AST go to
              http://esprima.org/demo/parse.html

   @end
   Created : 17 Aug 2013 by gordon@vixo.com
```erlang
    -module(to_js).

    -export([
             conv/2
            ]).

    -include("luvviescript.hrl").

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
```
 TODO understand the types of expression (eg ExpressionStatement)
```erlang
    conv({match, {_Line, none}, Left, Right}, Context) ->
        {
          {"type",     "AssignmentExpression"},
          {"operator", "="},
          {"left",     Left},
          {"right",    Right}
        };
    conv({singleton_fn, Fn}, _Context) ->
        Fn;
    conv(Node, Context) ->
        io:format("Node is:~n-~p~n Context is:~n-~p~n", [Node, Context]),
        Node.

    loc(Line, Start, End) ->
        {"loc", [{"start", [{"line", Line}, {"column", Start}]},
                 {"end",   [{"line", Line}, {"column", End}]}]}.
```
