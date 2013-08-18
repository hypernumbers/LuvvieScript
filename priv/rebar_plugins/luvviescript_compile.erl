%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       This script compiles Erlang to Javascript
%%%
%%% @end
%%% Created :  15 Aug 2013 by gordonguthrie@backawinner.gg

-module(luvviescript_compile).

-export([
         luvviescript_compile/2
        ]).

-define(JSDIR,       "js/").
-define(SUPPORTED,   "priv/supported/").
-define(UNSUPPORTED, "priv/unsupported/").

luvviescript_compile(_A, _B) ->
    case has_js_dir() of
        true  -> ok = clear_old_js();
        false -> ok
    end,
    ok = filelib:ensure_dir(?JSDIR),
    ok = compile().

compile() ->
    code:add_patha("ebin/"),
    {ok, Dir} = file:get_cwd(),
    Files = filelib:wildcard(Dir ++ "/test/supported/ty*.erl"),
    Output = [luvviescript:compile(File) || File <- Files],
    io:format("Output is ~p~n", [Output]),
    ok.

clear_old_js() ->
    case file:list_dir(?JSDIR) of
        {error, _}  -> ok; % directory doesn't exist, that's ok
        {ok, Files} -> [ok = file:delete(?JSDIR ++ X) || X <- Files],
                       ok
    end.

has_js_dir() ->
    case file:list_dir(?JSDIR) of
        {error, _} -> false;
        {ok, _}    -> true
    end.
