%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       This is a rebar plugin for compiling literate erlang
%%%
%%% @end
%%% Created :  2 Sep 2013 by gordon@vixo.com
-module(compile_literate).

-export([
         compile_literate/2
        ]).

compile_literate(Config, AppFile) ->
    App = filename:basename(AppFile),
    %% once you have run this rebar plugin once the AppFile changes :(
    case App of
        "luvviescript.app"     -> ok = compile_l();
        "luvviescript.app.src" -> ok = compile_l();
        _                      -> ok
    end.

compile_l() ->
    ErlOpts = rebar_config:get(Config, erl_opts, []),
    SrcDirs = get_src_dirs(ErlOpts),
    ok = clear_down(SrcDirs),
    CompilerOptions = get_compiler_options(ErlOpts),
    ErlFiles = [filelib:wildcard(X ++ "/../src_md/*.erl.md") || X <- SrcDirs],
    [ok = compile_file(X, CompilerOptions, erl) || X <- lists:merge(ErlFiles)],
    HrlFiles = [filelib:wildcard(X ++ "/../include_md/*.hrl.md") || X <- SrcDirs],
    [ok = compile_file(X, CompilerOptions, hrl) || X <- lists:merge(HrlFiles)],
    ok.

compile_file(File, CompilerOptions, Type) ->
    CWD = rebar_utils:get_cwd(),
    {ok, Lines} = read_lines(CWD ++ "/" ++ File),
    Source = make_erlang_source(Lines),
    ok = write_source_and_compile(Source, File, CompilerOptions, Type).

make_erlang_source(Lines) ->
    make_erl2(Lines, comment, []).

make_erl2([], _Type, Acc) ->
    lists:flatten(lists:reverse(Acc));
make_erl2(["```erlang" ++ _Rest | T], comment, Acc) ->
    make_erl3(T, erlang, ["%%%```erlang\n" | Acc]);
make_erl2([H | T], comment, Acc) ->
    make_erl2(T, comment, ["%%% " ++ H | Acc]).

make_erl3([], _Type, Acc) ->
    lists:flatten(lists:reverse(Acc));
make_erl3(["\n" | T], erlang, Acc) ->
    make_erl3(T, erlang, ["\n" | Acc]);
make_erl3(["    " ++ Rest | T], erlang, Acc) ->
    make_erl3(T, erlang, [Rest | Acc]);
make_erl3(["```" ++ _Rest | T], erlang, Acc) ->
    make_erl2(T, comment, ["%%%```\n" | Acc]);
%% Oops, not indented? lets comment out then
make_erl3(List, erlang, Acc) ->
    make_erl2(List, comment, Acc).

read_lines(File) ->
    case file:open(File, read) of
        {error, Err} -> {error, Err};
        {ok, Id}     -> read_l2(Id, [])
    end.

read_l2(Id, Acc) ->
    case file:read_line(Id) of
        {ok, Data}   -> read_l2(Id, [Data | Acc]);
        {error, Err} -> {error, Err};
        eof          -> {ok, lists:reverse(Acc)}
    end.

get_src_dirs(ErlOpts) ->
    case proplists:get_value(src_dirs, ErlOpts) of
        undefined -> ["src"];
        SrcDirs   -> SrcDirs
    end.

get_compiler_options(ErlOpts) ->
    proplists:delete(src_dirs, ErlOpts).

write_source_and_compile(Source, File, CompilerOptions, Type) ->
    File2 = filename:basename(filename:rootname(File)),
    Dir = filename:dirname(File),
    NewCompOpts = adjust_output_dirs(CompilerOptions, Dir),
    Dir2 = case Type of
               erl -> Dir  ++ "/../src/";
               hrl -> Dir  ++ "/../include/"
           end,
    ok = filelib:ensure_dir(Dir2),
    ok = file:write_file(Dir2 ++ File2, Source),
    case Type of
        erl -> os:cmd("cp " ++ Dir ++ "/*.app.src " ++ Dir2);
        hrl -> ok
    end,
    ok.

adjust_output_dirs(CompilerOptions, Dir) ->

    case proplists:is_defined(outdirs, CompilerOptions) of
        false ->
            OutputDir = "ebin/",
            filelib:ensure_dir(OutputDir),
            [{outdir, OutputDir} | CompilerOptions];
        true  ->
            CompilerOptions
    end.

clear_down(SrcDirs) ->
    WildCards = lists:merge([[
                              X ++ "/../include/*",
                              X ++ "/../src/*"
                             ] || X <- SrcDirs]),
    Files = lists:merge([filelib:wildcard(X) || X <- WildCards]),
    [ok = file:delete(X) || X <- Files],
    ok.
