    @author    Gordon Guthrie
    @copyright (C) 2013, Gordon Guthrie
    @doc       This is the luvviescript compiler
 
    @end
    Created : 17 Aug 2013 by gordon@vixo.com
```erlang
    -module(luvviescript).

    -export([
             compile/1,
             compile/2
            ]).

    -include("luvviescript.hrl").
```
```
 ```
  include the core erlang syntax records that we are going to act on
  this file is in /usr/local/lib/erlang/lib/compiler-N.N.N/src
  or the equivalent. That dir needs to be set in the rebar compiler
  options for this to compile
```erlang
    -include_lib("core_parse.hrl").

    compile(File) ->
        compile(File, production).

    compile(File, Environment) ->
        io:format("Compiling ~p~n", [File]),
        %% we are going to compile the .P2 version of the Erlang file
        %% not the plain one, so we create that version first
        {ok, DotP2} = make_dot_P2(File),
        {ok, _, Syntax} = compile_to_ast(File),
        ok = maybe_write(Environment, File, Syntax, ".ast"),
        #c_module{defs = Body} = Syntax,
        %% Erlang regards somefun/1 and somefun/N as two differnt
        %% fns, Javascript thinks they are they same.
        %% so we need to group multiple arity erlang fns.
        %% easiest way to do that is to sort the fns by name/arity
        SortFn = fun({Var1, _}, {Var2, _}) ->
                         Var1#c_var.name < Var2#c_var.name
                 end,
        Body2 = lists:sort(SortFn, Body),
        Syntax2 = Syntax#c_module{defs = Body2},
        ok = maybe_write(Environment, File, Syntax2, ".ast2"),
        %% we are going to use a .P file as the souce file for the purposes of
        %% having a source map. .P files have predictable and normalised layout
        %% which makes it possible to use them to collect column information
        %% for the source map file
        {ok, Tokens, _} = erl_scan:string(lists:flatten(DotP2), 1,
                                          [
                                           return_white_spaces,
                                           return_comments,
                                           text
                                          ]),
        ok = maybe_write(Environment, File, Tokens, ".tks"),
        {ok, Tokens2} = collect_tokens(Tokens),
        ok = maybe_write(Environment, File, Tokens2, ".tks2"),
        Body3 = merge(Syntax2#c_module.defs, Tokens2, []),
        Syntax3 = fix_exports(Syntax2#c_module{defs = Body3}),
        ok = maybe_write(Environment, File, Syntax3, ".ast3"),
        Jast = to_jast:conv(Syntax3),
        ok = maybe_write(Environment, File, Jast, ".jast").

    merge([], _Tokens, Acc) ->
        lists:reverse(Acc);
```
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
        {NewVars, NewToks, Context} = get_new_vars(Vars, Tokens),
        {{Line, Offset}, NewToks2} = get_details(NewToks, Line, Name),
        {NewBody, NewToks3} = merge_body(Body, NewToks2, Context),
        NewFn = set_col(Offset, Fn#c_fun{vars = NewVars, body = NewBody}),
        {[NewFn | Acc], NewToks3}.

    merge_body(#c_literal{anno = []} = CLit, Tokens, _Context) ->
        {CLit, Tokens};
    merge_body(#c_literal{val = Val} = CLit, Tokens, _Context) ->
        Line = get_line_var(CLit),
        {{Line, Offset}, NewToks} = get_details(Tokens, Line, Val),
        NewLit = set_col(Offset, CLit),
        {NewLit, NewToks};
    merge_body(Body, Tokens, _Context) ->
        io:format("in merge_body Skipping ~p~n", [Body]),
        {Body, Tokens}.

    get_new_vars([], Tokens) ->
        {[], Tokens, []};
    get_new_vars([H | _T] = List, Tokens) ->
        Line = get_line_var(H),
        {Line, Tks} = lists:keyfind(Line, 1, Tokens),
        Matches = get_matches(Tks, []),
        NewToks = lists:keydelete(Line, 1, Tokens),
        {NewVars, Context} = get_new_vars2(List, Matches, [], []),
        {NewVars, NewToks, Context}.

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

    make_dot_P2(File) ->
        IncludeDir = filename:dirname(File) ++ "/../include",
        DebugDir   = filename:dirname(File) ++ "/../debug",
        PDir       = filename:dirname(File) ++ "/../psrc",
        case compile:file(File, [
                                 'P',
                                 {i,      IncludeDir},
                                 {outdir, DebugDir}
                                ]) of
            {ok, []} -> File2 = filename:rootname(filename:basename(File)) ++ ".P",
                        {ok, P2} = case file:open(DebugDir ++ "/" ++ File2, read) of
                                       {error, Err} -> exit(Err);
                                       {ok, ID}     -> FileNameFlag = false,
                                                       scan(ID, FileNameFlag, [])
                                   end,
                        File3 = filename:rootname(filename:basename(File)) ++ ".erl",
                        ok = write_file(P2, PDir ++ "/" ++ File3),
                        {ok, P2};
            error    -> io:format("Cannae compile ~p~n", [File]),
                        {error, cant_compile}
        end.

    write_file(List, File) ->
        _Ret = filelib:ensure_dir(File),
        case file:open(File, [append]) of
            {ok, Id} -> write_f2(List, Id),
                        file:close(Id),
                        ok;
            _        -> error
        end,
        ok.

    write_f2([],      _Id) -> ok;
    write_f2([H | T], Id)  -> io:fwrite(Id, "~s", [H]),
                               write_f2(T, Id).

    scan(ID, Flag, Acc) ->
        case {file:read_line(ID), Flag} of
             {{ok, "-file(" ++ _Rest = F}, false} -> scan(ID, true, [F | Acc]);
             {{ok, "-file(" ++ _Rest},     true}  -> scan(ID, Flag, Acc);
             {{ok, Line},                  _}     -> scan(ID, Flag, [Line | Acc]);
             {eof,                         _}     -> Rev = lists:reverse(Acc),
                                                     {ok, Rev}
         end.

    compile_to_ast(File) ->
        IncludeDir = filename:dirname(File) ++ "../include",
        PDir       = filename:dirname(File) ++ "/../psrc",
        File2 = PDir ++ "/" ++ filename:rootname(filename:basename(File)),
        compile:file(File2, [{i, IncludeDir}, binary, to_core]).

    collect_tokens(List) -> {ok, col2(List, 1, 1, [], [])}.

    col2([], Line, Indent,  A1, A2) ->
        {Entry, _NewIndent} = make_entry(lists:reverse(A1), Indent, []),
        lists:reverse([{Line, Entry} | A2]);
    col2([H  | T], Line, Indent, A1, A2) ->
        Details = element(2, H),
        Ln = get_line(Details),
        case Line of
            Ln -> col2(T, Ln, Indent, [H | A1], A2);
            _  -> {Entry, NewIndent} = make_entry(lists:reverse(A1), Indent, []),
                  col2([H | T], Ln, NewIndent, [], [{Line, Entry} | A2])
        end.

    make_entry([], Indent, Acc) ->
        {lists:reverse(Acc), Indent};
    make_entry([{Operator, Details} | T], Indent, Acc) ->
        {Loc, NewIndent} = make_location(Details, Indent),
        NewAcc = {Operator, Loc, operator},
        make_entry(T, NewIndent, [NewAcc | Acc]);
    make_entry([{white_space, Details, WS} | T], Indent, Acc) ->
        %% if whitespace starts with a new line it is a terminal whitespace
        %% so reset indent counter to it (ie don't add it to Indent)
        %% otherwise business as usual...
        [{line, Line}, {text, Txt}] = Details,
        [H | _] = Txt,
        NewIndent = case H of
                        $\n -> length(Txt);
                        _  -> length(Txt) + Indent
                    end,
        NewAcc = {WS, {Line, Indent}, white_space},
        make_entry(T, NewIndent, [NewAcc | Acc]);
```
```
  ```
  comments are either whole line (don't care about the length)
  or at the end of a line followed by whitespace (don't care about the length).
```erlang
    make_entry([{comment, _, _} | T], Indent, Acc) ->
        make_entry(T, Indent, Acc);
    make_entry([{Type, Details, Thing} | T], Indent, Acc) ->
        {Loc, NewIndent} = make_location(Details, Indent),
        NewAcc = {Thing, Loc, Type},
        make_entry(T, NewIndent, [NewAcc | Acc]).

    get_line([{line, Ln}, _]) -> Ln.

    make_location([{line, Ln}, {text, Txt}], Indent) ->
        End = Indent + length(Txt),
        {{Ln, {Indent, End}}, End + 1}.

    maybe_write(production, _, _, _) ->
        ok;
    maybe_write(debug, File, Contents, FileType) ->
        OutputDir = filename:dirname(File) ++ "/../debug/",
        write(OutputDir, File, Contents, FileType).

    write(Dir, File, Contents, FileType) ->
        OutputFile = filename:rootname(filename:basename(File)) ++ FileType,
        _Return = filelib:ensure_dir(File),
        ok = make_utils:write_file(Contents, Dir ++ OutputFile).

    get_line_var(Rec) when is_tuple(Rec) ->
        Attrs = element(2, Rec),
        FilterFn = fun(X) when is_integer(X) -> true;
                      (_X)                   -> false
                   end,
        [N] = lists:filter(FilterFn, Attrs),
        N.

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

    fix_exports(#c_module{exports = Exps} = CMod) ->
        FilterFn = fun(#c_var{name = {module_info, _}}) -> false;
                      (_)                               -> true
                   end,
        NewExps = lists:filter(FilterFn, Exps),
        CMod#c_module{exports = NewExps}.

```
```
