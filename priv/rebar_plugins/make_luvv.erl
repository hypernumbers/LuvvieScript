%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       This script compiles Erlang to Javascript
%%%
%%% @end
%%% Created :  15 Aug 2013 by gordonguthrie@backawinner.gg

-module(make_luvv).

-export([
         make_luvv/2,
         make_tests/2
        ]).

-define(JSDIR,       "js/").


make_tests(_A, _B) ->
    io:format("in make tests...~n"),
    ok.

make_luvv(_A, _B) ->
    Dirs = ["test/passing", "test/not_passing"],
    [compile(X) || X <- Dirs],
    ok.

compile(Dir) ->
    SubDirs = ["/js", "/pbin"],
    [do_housekeeping(Dir ++ X) || X <- SubDirs],
    code:add_patha("ebin/"),
    Dir2 = Dir  ++ "/src/",
    Files = filelib:wildcard(Dir2 ++ "*.erl"),
    [ok = output(File) || File <- Files],
    ok.

output(File) ->
    Output = luvviescript:compile(File),
    OutputFile = filename:basename(filename:rootname(File)) ++ ".js",
    OutputDir = filename:dirname(File) ++ "/../js/",
    ok = write_file(Output, OutputDir ++ OutputFile),
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
        false -> filelib:ensure_dir(Dir)
    end.

write_file(String, File) ->
    _Return = filelib:ensure_dir(File),
    case file:open(File, [append]) of
        {ok, Id} ->
            io:fwrite(Id, "~s~n", [String]),
            file:close(Id);
        _ ->
            error
    end.
