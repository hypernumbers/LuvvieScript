   @author    Gordon Guthrie
   @copyright (C) 2014, Gordon Guthrie
   @doc       This module compiles the base Erlang to core
              AST, dotP2, etc, etc

   @end
   Created : 10th January 2014 by gordon@vixo.com
```erlang
    -module(lcompile).

    -export([
             to_ast/1,
             make_dot_P2/1
             ]).

    to_ast(File) ->
        IncludeDir = filename:dirname(File) ++ "../include",
        PDir       = filename:dirname(File) ++ "/../psrc",
        File2 = PDir ++ "/" ++ filename:rootname(filename:basename(File)),
        compile:file(File2, [{i, IncludeDir}, binary, to_core]).

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
```
