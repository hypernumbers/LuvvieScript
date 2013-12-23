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
 include the core erlang syntax records that we are going to act on
 this file is in /usr/local/lib/erlang/lib/compiler-N.N.N/src
 or the equivalent. That dir needs to be set in the rebar compiler
 options for this to compile
```erlang
    -include_lib("core_parse.hrl").

    -define(LINEENDING, {line_ending, ";", nonce}).
    -define(INDENT, "    ").

    compile(File) ->
        compile(File, production).

    compile(File, Environment) ->
        io:format("Compiling ~p~n", [File]),
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
        {ok, DotP2} = make_dot_P2(File),
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
        Syntax3 = Syntax2#c_module{defs = Body3},
        ok = maybe_write(Environment, File, Syntax3, ".ast3"),
        ok.

    merge([], _Tokens, Acc) ->
        lists:reverse(Acc);
```
 drop the module_info fns
```erlang
    merge([{#c_var{name = {module_info, _}}, _} | T], Tokens, Acc) ->
        merge(T, Tokens, Acc);
    merge([{#c_var{name = {Name, _}} = First, #c_fun{} = Fn} | T], Tokens, Acc) ->
        io:format("Merging the body of ~p~n", [First]),
        NewFn = merge_fn(Name, Fn, Tokens, []),
        merge(T, Tokens, [{First, NewFn} | Acc]);
    merge([H | T], Tokens, Acc) ->
        io:format("In merge Skipping ~p~n", [H]),
        merge(T, Tokens, Acc).

    merge_fn(Name, #c_fun{body = Body} = Fn, Tokens, Acc) ->
        Line = get_line_var(Body),
        io:format("Line is ~p~n-Name is ~p~n-Tokens is ~p~n", [Line, Name, Tokens]),
        {Line2, NewTokens} = get_details(Tokens, Line, Name),
        io:format("Line2 is ~p~n", [Line2]),
        NewBody = merge_body(Body, Tokens, []),
        [Fn#c_fun{body = NewBody} | Acc].

    merge_body(Body, Tokens, Acc) ->
        io:format("in merge_body Skipping ~p~n", [Body]),
        Acc.

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

        make_dot_P2(File) ->
            IncludeDir = filename:dirname(File) ++ "/../include",
            PDir       = filename:dirname(File) ++ "/../psrc",
            case compile:file(File, [
                                     'P',
                                     {i,      IncludeDir},
                                     {outdir, PDir}
                                    ]) of
                {ok, []} -> File2 = filename:rootname(filename:basename(File)) ++ ".P",
                            {ok, P2} = case file:open(PDir ++ "/" ++ File2, read) of
                                           {error, Err} -> exit(Err);
                                           {ok, ID}     -> FileNameFlag = false,
                                                           scan(ID, FileNameFlag, [])
                                       end,
                            File3 = filename:rootname(filename:basename(File)) ++ ".P2",
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
        compile:file(File, [{i, IncludeDir}, binary, to_core]).

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
        OutputFile = filename:rootname(filename:basename(File)) ++ FileType,
        _Return = filelib:ensure_dir(File),
        ok = make_utils:write_file(Contents, OutputDir ++ OutputFile).

    get_line_var(Rec) when is_tuple(Rec) ->
        Attrs = element(2, Rec),
        FilterFn = fun(X) when is_integer(X) -> true;
                      (_X)                   -> false
                   end,
        [N] = lists:filter(FilterFn, Attrs),
        N.
```
