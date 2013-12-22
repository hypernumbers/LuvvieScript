```erlang
    -module(core_compile).

    -export([
             compile/1
            ]).

```
 debugging
```erlang
    -export([
             test/0
            ]).

    test() ->
        Dir = "../test/not_passing/src/",
        File = "basic_types.core",
        {ok, Core} = compile(Dir ++ File),
        File2 = "basic_types.cast",
        write_file([Core], Dir ++ File2).

    compile(File) ->
        case file:read_file(File) of
            {ok,Bin} ->
                case core_scan:string(binary_to_list(Bin)) of
                    {ok,Toks,_} ->
                        case core_parse:parse(Toks) of
                            {ok, Mod} ->
                                {ok, Mod};
                            {error,E} ->
                                {error, {parse, E}}
                        end;
                    {error,E,_} ->
                        {error, {scan, E}}
                end;
            {error,E} ->
                {error,{read, E}}
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
    write_f2([H | T], Id)  -> io:fwrite(Id, "~p", [H]),
                              write_f2(T, Id).
```
