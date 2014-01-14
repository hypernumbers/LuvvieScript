%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       This is a rebar plugin for turning Erlang source
%%%            into literate Erlang
%%%
%%% @end
%%% Created :  2 Sep 2013 by gordon@vixo.com
-module(markup).

-export([
         markup/2
        ]).

markup(Config, AppFile) ->
    App = filename:basename(AppFile),
    case App of
        "luvviescript.app.src" ->
            ErlOpts = rebar_config:get(Config, erl_opts, []),
            SrcDirs = get_src_dirs(ErlOpts),
            ErlFiles = get_files(SrcDirs, erl),
            HrlFiles = get_files(SrcDirs, hrl),
            [ok = markup_to_literate(X, erl) || X <- ErlFiles],
            [ok = markup_to_literate(X, hrl) || X <- HrlFiles],
            ok;
        _ ->
            ok
    end.

get_files(SrcDirs, Type) ->
    WildCards = case Type of
                    erl -> "/../src_md/.erl/*.erl";
                    hrl -> "/../include_md/.hrl/*.hrl"
                end,
    Files = lists:merge([filelib:wildcard(X ++ WildCards) || X <- SrcDirs]),
    FilterFun = fun(X) ->
                        not filelib:is_dir(X)
                end,
    lists:filter(FilterFun, Files).

markup_to_literate(File, Type) ->
    CWD = rebar_utils:get_cwd(),
    {ok, Lines} = read_lines(CWD ++ "/" ++ File),
    Source = make_markdown_source(Lines),
    ok = write_source(Source, File, Type).

make_markdown_source(Lines) ->
    make_markdown(Lines, []).

make_markdown([], Acc) ->
    lists:flatten(lists:reverse(Acc));
make_markdown(["%%%```erlang" ++ _Rest | T], Acc) ->
    make_erlang(T, ["```erlang\n" | Acc]);
make_markdown(["%%```erlang" ++ _Rest | T], Acc) ->
    make_erlang(T, ["```erlang\n" | Acc]);
make_markdown(["%```erlang" ++ _Rest | T], Acc) ->
    make_erlang(T, ["```erlang\n" | Acc]);
%% order matters!
make_markdown(["%%%" ++ Rest | T], Acc) ->
    make_markdown(T, [Rest | Acc]);
make_markdown(["%%" ++ Rest | T], Acc) ->
    make_markdown(T, [Rest | Acc]);
make_markdown(["%" ++ Rest | T], Acc) ->
    make_markdown(T, [Rest | Acc]);
make_markdown(["\n" | T], Acc) ->
    make_markdown(T, ["\n" | Acc]);
make_markdown([H | T], Acc) ->
    make_erlang([H | T], ["```erlang\n" | Acc]).

make_erlang([], Acc) ->
    lists:flatten(lists:reverse(["```\n" |  Acc]));
make_erlang(["\n" | T], Acc) ->
    make_erlang(T, ["\n" | Acc]);
%% order matters!
make_erlang(["%%%" ++ Rest | T], Acc) ->
    make_markdown(T, [Rest, "```\n" | Acc]);
make_erlang(["%%" ++ Rest | T], Acc) ->
    make_markdown(T, [Rest, "```\n" | Acc]);
make_erlang(["%" ++ Rest | T], Acc) ->
    make_markdown(T, [Rest, "```\n" | Acc]);
make_erlang([H | T], Acc) ->
    make_erlang(T, ["    " ++ H | Acc]).

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

write_source(Source, File, Type) ->
    File2 = filename:basename(File) ++ ".md",
    Dir = case Type of
              erl -> filename:dirname(File) ++ "/../../src_md/";
              hrl -> filename:dirname(File) ++ "/../../include_md/"
          end,
    ok = filelib:ensure_dir(Dir),
    ok = file:write_file(Dir ++ File2, Source).


