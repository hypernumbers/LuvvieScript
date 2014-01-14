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
        %% for debugging we write out the source code of the Erlang core
        %% file whose AST we will be transforming
        ok = maybe_core(Environment, File),
        %% we are going to compile the .P2 version of the Erlang file
        %% not the plain one, so we create that version first
        {ok, DotP2} = lcompile:make_dot_P2(File),
        {ok, _, Syntax} = lcompile:to_ast(File),
        ok = maybe_write(Environment, File, Syntax, ".ast", term),
        #c_module{defs = Body} = Syntax,
        %% Erlang regards somefun/1 and somefun/N as two different
        %% fns, Javascript thinks they are they same.
        %% so we need to group multiple arity erlang fns.
        %% easiest way to do that is to sort the fns by name/arity
        SortFn = fun({Var1, _}, {Var2, _}) ->
                         Var1#c_var.name < Var2#c_var.name
                 end,
        Body2 = lists:sort(SortFn, Body),
        Syntax2 = Syntax#c_module{defs = Body2},
        ok = maybe_write(Environment, File, Syntax2, ".ast2", term),
        %% we are going to use a .P file as the souce file for the purposes of
        %% having a source map. .P files have predictable and normalised layout
        %% which makes it easier to use them to collect column information
        %% for the source map file
        {ok, Tokens, _} = erl_scan:string(lists:flatten(DotP2), 1,
                                          [
                                           return_white_spaces,
                                           return_comments,
                                           text
                                          ]),
        ok = maybe_write(Environment, File, Tokens, ".tks", term),
        %% now we collect all the tokens with the line/col information
        {ok, Tokens2} = tokens:collect(Tokens),
        ok = maybe_write(Environment, File, Tokens2, ".tks2", term),
        %% now transform the Erlang core AST file by merging in the column
        %% information from the collected tokens so that we can write the
        %% Javascript AST with the right info to get a Source Map
        Body3 = merge:add_line_info(Syntax2#c_module.defs, Tokens2),
        Syntax3 = fix_exports(Syntax2#c_module{defs = Body3}),
        ok = maybe_write(Environment, File, Syntax3, ".ast3", term),
        %% finally we can start coverting Erlang (core) to Javascript
        Jast = from_core:conv(Syntax3),
        ok = maybe_write(Environment, File, Jast, ".jast", term),
        %% ok = debug_json:debug(Jast),
        Jast2 = io_lib:format("~s", [rfc4627:encode(Jast)]),
        ok = maybe_write(Environment, File, Jast2, ".jast2", string),
        %% life is easier for everyone if the json we output is actually
        %% readable so make it so
        ok = pretty_print_json(File),
        %% now turn our json AST for Javascript into actual Javascript
        ok = make_javascript(File),
        ok.

    make_javascript(File) ->
        DirIn      = filename:dirname(File) ++ "/../debug/",
        DirOut     = filename:dirname(File) ++ "/../js/",
        SrcDir     = filename:dirname(File) ++ "/../psrc/",
        FileRoot   = filename:rootname(filename:basename(File)),
        FileIn     = DirIn  ++ FileRoot ++ ".json",
        FileOut    = DirOut ++ FileRoot ++ ".js",
        SourceMap  = DirOut ++ FileRoot ++ ".js.map",
        SourceFile = SrcDir ++ FileRoot ++ ".erl",
        CodeGen    = "/usr/local/bin/escodegen-cl2",
        Cmd = CodeGen
            ++ " --js_ast "      ++ FileIn
            ++ " --js_output "   ++ FileOut
            ++ " --source_file " ++ SourceFile
            ++ " --source_map "  ++ SourceMap,
        case os:cmd(Cmd) of
            []  -> ok; % fine and doody
            Msg -> io:format("Invalid JSON AST for ~p~n" ++ Msg, [FileOut])
        end,
        ok.

    pretty_print_json(File) ->
        Dir      = filename:dirname(File) ++ "/../debug/",
        FileRoot = Dir ++ filename:rootname(filename:basename(File)),
        FileIn   = FileRoot ++ ".jast2",
        FileOut  = FileRoot ++ ".json",
        PP       = "priv/json-prettyprinter/prettyjson.py",
        [] = os:cmd("cat " ++ FileIn ++ " | " ++ PP ++ " > " ++ FileOut),
        ok.

    maybe_core(production, _) ->
        ok;
    maybe_core(debug, File) ->
        IncDir  = filename:dirname(File) ++ "../include",
        ODir    = filename:dirname(File) ++ "/../debug/",
        {ok, _} = compile:file(File, [{i, IncDir}, {outdir, ODir}, to_core]),
        ok.

    maybe_write(production, _, _, _, _) ->
        ok;
    maybe_write(debug, File, Contents, FileType, Format) ->
        OutputDir = filename:dirname(File) ++ "/../debug/",
        write(OutputDir, File, Contents, FileType, Format).

    write(Dir, File, Contents, FileType, Format) ->
        OutputFile = filename:rootname(filename:basename(File)) ++ FileType,
        _Return = filelib:ensure_dir(File),
        ok = make_utils:write_file(Contents, Dir ++ OutputFile, Format).

    fix_exports(#c_module{exports = Exps} = CMod) ->
        FilterFn = fun(#c_var{name = {module_info, _}}) -> false;
                      (_)                               -> true
                   end,
        NewExps = lists:filter(FilterFn, Exps),
        CMod#c_module{exports = NewExps}.

```
```
