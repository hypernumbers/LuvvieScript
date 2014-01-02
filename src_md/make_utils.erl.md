  @author vagrant <vagrant@erlang-wtd.dev>
  @copyright (C) 2013, vagrant
  @doc utils for rebar to call from plugins

  @end
  Created :  5 Sep 2013 by vagrant <vagrant@erlang-wtd.dev>

```erlang
    -module(make_utils).

    -export([
             compile/1,
             compile/2,
             make_tests/1,
             write_file/3
            ]).

```
```
 for debugging stuff
```erlang
    -export([
             plain_log/2
            ]).

    make_tests(Dir) ->
        SubDirs = ["/ebin", "/debug"],
        [do_housekeeping(Dir ++ X) || X <- SubDirs],
        code:add_patha(Dir ++ "/ebin/"),
        Dir2 = Dir  ++ "/src/",
        Files = filelib:wildcard(Dir2 ++ "*.erl"),
        [ok = compile_erlang(File) || File <- Files],
        Modules = [list_to_atom(filename:rootname(filename:basename(X)))
                   || X <- Files],
        ok = load_beam_files(Modules),
        ok = make_compile_tests(Modules, Dir),
        Tests = [get_tests(X) || X <- Modules],
        io:format("Gonnae write the tests then son?~n"),
        io:format("Tests is ~p~n", [Tests]),
        Results = [get_results(X) || X <- Tests],
        io:format("Results is ~p~n", [Results]),
        ok.

    make_compile_tests(Modules, Dir) ->
        Dir2 = filename:basename(Dir),
        Name = Dir2 ++ "_compile_SUITE",
        Tests = ["?TESTFN("
                 ++ "'" ++ atom_to_list(X) ++ "_test', "
                 ++ Dir2 ++ ", "
                 ++ "'" ++ atom_to_list(X) ++ "'"
                 ++ ").\n"
                 || X <- Modules],
        All = string:join(["'" ++ atom_to_list(X) ++ "_test'"
                           || X <- Modules], ",\n"),
        All2 = "all() ->\n[\n" ++ All ++ "\n].\n",
        Suite = lists:flatten([
                               "%%% this file is GENERATED - DO NOT EDIT\n",
                               "-module(" ++ Name ++ ").\n",
                               "\n",
                               "-include(\"test_compile_hdr.part\").\n",
                               "\n",
                               All2,
                               "\n"
                               | Tests
                              ]),
        ok = file:write_file("test/" ++ Name ++ ".erl", Suite).

    get_results({Mod, Fns}) ->
        {Mod, [{X, Mod:X()} || X <- Fns]}.

    get_tests(Module) ->
        Funs = Module:module_info(exports),
        {Module, [Fn || {Fn, 0} <- Funs,
                        Fn /= module_info]}.

    load_beam_files([]) ->
        ok;
    load_beam_files([H | T]) ->
        {module, H} = code:load_file(H),
        load_beam_files(T).

    compile_erlang(File) ->
        io:format("Compiling ~p~n", [File]),
        IncludeDir  = filename:dirname(File) ++ "/../include",
        OutDir      = filename:dirname(File) ++ "/../ebin",
        {ok, _} = compile:file(File, [
                                      {i,      IncludeDir},
                                      {outdir, OutDir}
                                     ]),
        ok.

    compile(Dir) ->
        compile(Dir, production).

    compile(Dir, Environment) ->
        SubDirs = ["/js", "/psrc", "/debug"],
        [do_housekeeping(Dir ++ X) || X <- SubDirs],
        code:add_patha("ebin/"),
        Dir2 = Dir  ++ "/src/",
        Files = filelib:wildcard(Dir2 ++ "*.erl"),
        [ok = luvviescript:compile(File, Environment) || File <- Files],
        ok.

    clear_old_files(Dir) ->
        case file:list_dir(Dir) of
            {error, _}  -> ok; % directory doesn't exist, that's ok
            {ok, Files} -> [ok = file:delete(Dir ++ "/" ++ X) || X <- Files],
                           ok
        end.

    has_dir(Dir) ->
        case file:list_dir(Dir) of
            {error, _} -> false;
            {ok, _}    -> true
        end.

    do_housekeeping(Dir) ->
        case has_dir(Dir) of
            true  -> ok = clear_old_files(Dir);
            false -> filelib:ensure_dir(Dir ++ "/nonce.file")
        end.

    write_file(Term, File, term) ->
        write_f2(Term, File, "~p~n");
    write_file(Term, File, string) ->
    write_f2(Term, File, "~s~n").

    write_f2(Term, File, Format) ->
        _Return = filelib:ensure_dir(File),
        case file:open(File, [write]) of
            {ok, Id} ->
                io:fwrite(Id, Format, [Term]),
                file:close(Id);
            _ ->
                error
        end.

    plain_log(String, File) ->
        _Return = filelib:ensure_dir(File),

        case file:open(File, [append]) of
            {ok, Id} ->
                io:fwrite(Id, "~s~n", [String]),
                file:close(Id);
            _ ->
                error
        end.
```
