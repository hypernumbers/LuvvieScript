   @author    Gordon Guthrie
   @copyright (C) 2013, Gordon Guthrie
   @doc       This module merges the line/column information
              into the core Erlang AST

   @end
   Created : 10th January 2014 by gordon@vixo.com
```erlang
    -module(merge).

    -export([
             add_line_info/2
            ]).

```
```
  include the core erlang syntax records that we are going to act on
  this file is in /usr/local/lib/erlang/lib/compiler-N.N.N/src
  or the equivalent. That dir needs to be set in the rebar compiler
  options for this to compile
```erlang
    -include_lib("core_parse.hrl").

    add_line_info(Syntax, Tokens) ->
        merge(Syntax, Tokens, []).

    merge([], _Tokens, Acc) ->
        lists:reverse(Acc);
```
```
  drop the module_info fns
```erlang
    merge([{#c_var{name = {module_info, _}}, _} | T], Tokens, Acc) ->
        merge(T, Tokens, Acc);
    merge([{#c_var{name = {Name, _}} = First, #c_fun{} = Fn} | T], Tokens, Acc) ->
        {NewFn, NewToks} = merge_fn(Name, Fn, Tokens, []),
        merge(T, NewToks, [{First, NewFn} | Acc]);
    merge([H | T], Tokens, Acc) ->
        io:format("In merge Skipping ~p~n", [H]),
        merge(T, Tokens, Acc).

    merge_fn(Name, #c_fun{vars = Vars, body = Body} = Fn, Tokens, Acc) ->
        Line = get_line_var(Fn),
        {NewVars, NewToks, _Context} = get_new_vars(Vars, Tokens),
        {{Line, Offset}, NewToks2} = get_details(NewToks, Line, Name),
        {NewBody, NewToks3} = merge_body(Body, NewToks2, []),
        NewFn = set_col(Offset, Fn#c_fun{vars = NewVars, body = NewBody}),
        {[NewFn | Acc], NewToks3}.

    merge_body([], Tokens, _Context) ->
        {[], Tokens};
    merge_body(#c_literal{anno = []} = CLit, Tokens, _Context) ->
        {CLit, Tokens};
    merge_body(#c_literal{val = Val} = CLit, Tokens, _Context) ->
        Line = get_line_var(CLit),
        {{Line, Offset}, NewToks} = get_details(Tokens, Line, Val),
        NewLit = set_col(Offset, CLit),
        {NewLit, NewToks};
    merge_body(#c_let{vars = Vars, body = Body} = CLet, Tokens, _Context) ->
        {NewVars, NewToks, NewContext} = get_new_vars(Vars, Tokens),
        {NewBody, NewToks2} = merge_body(Body, NewToks, NewContext),
        CLet2 = CLet#c_let{vars = NewVars, body = NewBody},
        {CLet2, NewToks2};
    merge_body(#c_apply{op = Op, args = Args} = CApp, Tokens, Context)
      when is_record(Op, c_var) ->
        {NewOp, NewToks} = merge_var(Op, Tokens, Context),
        {NewArgs, NewToks2} = merge_args(Args, NewToks, Context),
        {CApp#c_apply{op = NewOp, args = NewArgs}, NewToks2};
    merge_body(Body, Tokens, _Context) ->
        io:format("in merge_body Skipping ~p~n", [Body]),
        {Body, Tokens}.

    merge_var(#c_var{name = {Name, _}} = Var, Tokens, _Context) ->
        case get_line_var(Var) of
            none -> {Var, Tokens};
            Line -> {Line, Tks} = lists:keyfind(Line, 1, Tokens),
                    Match = lists:keyfind(Name, 1, Tks),
                    {_, {_, Offset}, _} = Match,
                    NewVar = set_col(Offset, Var),
                    NewTks = lists:keydelete(Name, 1, Tks),
                    NewTokens = lists:keystore(Line, 1, Tokens, {Line, NewTks}),
                    {NewVar, NewTokens}
        end.

    merge_args(Args, Tokens, Context) ->
        merge_a2(Args, Tokens, Context, []).

    merge_a2([], Tokens, _Context, Acc) ->
        {lists:reverse(Acc), Tokens};
    merge_a2([H | T], Tokens, Context, Acc) ->
        {NewAcc, NewToks} = merge_var(H, Tokens, Context),
        merge_a2(T, NewToks, Context, [NewAcc | Acc]).

    get_new_vars([], Tokens) ->
        {[], Tokens, []};
    get_new_vars([H | _T] = List, Tokens) ->
        case get_line_var(H) of
            none -> {List, Tokens, []};
            Line -> {Line, Tks} = lists:keyfind(Line, 1, Tokens),
                    Matches = get_matches(Tks, []),
                    NewToks = lists:keydelete(Line, 1, Tokens),
                    {NewVars, Context} = get_new_vars2(List, Matches, [], []),
                    {NewVars, NewToks, Context}
        end.

    get_new_vars2([], _Matches, Context, Acc) ->
        {lists:reverse(Acc), lists:reverse(Context)};
    get_new_vars2([H1 | T1], [H2 | T2], Context, Acc) ->
        Offset = get_first_offset(H2),
        NewH1 = set_col(Offset, H1),
        NewContext = [{H1#c_var.name, H2} | Context],
        get_new_vars2(T1, T2, NewContext, [NewH1 | Acc]).

    get_first_offset({_, Offset}) ->
        Offset;
    get_first_offset([{_, Offset} | _T]) ->
        Offset.

    get_details(Tokens, Ln, Name) ->
         case lists:keyfind(Ln, 1, Tokens) of
             false ->
                 {{Ln, none}, Tokens};
             {Ln, Tks} ->
                 case lists:keyfind(Name, 1, Tks) of
                     false ->
                         {{Ln, none}, Tokens};
                     {Name, Details, _} ->
                         NewTks = lists:keydelete(Name, 1, Tks),
                         NewTokens = lists:keystore(Ln, 1, Tokens, {Ln, NewTks}),
                         {Details, NewTokens}
                 end
         end.

```
```
  ```
  Core Erlang uses 'made up' variable names in its function definitions
  We need to match these variable names to the ones used in the source
  code so that we can build a source map
  here we are stepping through the tokens of a function defintion
  to build the context
```erlang
    get_matches([], Acc) ->
        lists:reverse(Acc);
    get_matches([{'{', _, _} | T], Acc) ->
        {NewTail, NewAcc} = grab_tuple(T, Acc),
        get_matches(NewTail, NewAcc);
    get_matches([{'[', _, _} | T], Acc) ->
        {NewTail, NewAcc} = grab_list(T, Acc),
        get_matches(NewTail, NewAcc);
    get_matches([{Var, {_, Offset}, var} | T], Acc) ->
        get_matches(T, [{Var, Offset} | Acc]);
    get_matches([_H | T], Acc) ->
        get_matches(T, Acc).

    grab_tuple(List, Acc) ->
        grab(List, '}', Acc).

    grab_list(List, Acc) ->
        grab(List, ']', Acc).

    grab([{CloseToken, _, _} | Tail], CloseToken, Acc) ->
        {Tail, [lists:reverse(Acc)]};
    grab([{Var, {_, Offset}, var} | T], CloseToken, Acc) ->
        grab(T, CloseToken, [{Var, Offset} | Acc]);
    grab([_H | T], CloseToken, Acc) ->
        grab(T, CloseToken, Acc).

    get_line_var(Rec) when is_tuple(Rec) ->
        Attrs = element(2, Rec),
        case Attrs of
            [] -> none;
            A  -> FilterFn = fun(X) when is_integer(X) -> true;
                                (_X)                   -> false
                             end,
                  [N] = lists:filter(FilterFn, A),
                  N
        end.

    set_col(none,         Rec) when is_tuple(Rec) -> Rec;
    set_col({_N, none},   Rec) when is_tuple(Rec) -> Rec;
    set_col({Start, End}, Rec) when is_tuple(Rec) ->
        Line = get_line_var(Rec),
        Attrs = element(2, Rec),
        Loc = {"loc", {obj, [
                             {"start", {obj, [
                                              {"line",   Line},
                                              {"column", Start}
                                             ]
                                       }
                             },
                             {"end", {obj, [
                                            {"line",   Line},
                                            {"column", End}
                                           ]
                                        }
                             }
                            ]
                      }
              },
        NewAttrs = [Loc | Attrs],
        setelement(2, Rec, NewAttrs).
```
