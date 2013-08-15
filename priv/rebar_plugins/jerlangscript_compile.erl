%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       This script compiles Erlang to Javascript
%%%
%%% @end
%%% Created :  15 Aug 2013 by gordonguthrie@backawinner.gg

-module(jerlangscript_compile).

-export([
         jerlangscript_compile/2
        ]).

-define(JSDIR,       "js/").
-define(SUPPORTED,   "priv/supported/").
-define(UNSUPPORTED, "priv/unsupported/").

jerlangscript_compile(_A, _B) ->
    case has_js_dr() of
        true  -> ok = clear_old_js(),
                 ok = filelib:ensure_dir(?JSDIR),
                 ok = compile();
        false -> ok
    end.

compile() ->
    code:add_patha("ebin/"),
    {ok, Dir} = file:get_cwd(),
    Files = filelib:wildcard(Dir ++ "/test/supported/*.erl"),
    SyntaxFiles = [{X, compile_to_ast(X)} || X <- Files],
    Output = [jerlangscript:compile(S, File)
              || {File, {ok, _, S}} <- SyntaxFiles],
    io:format("Output is ~p~n", [Output]),
    ok.

compile_to_ast(File) -> case compile:file(File, [to_pp, binary]) of
                            {ok, _, _} = Syn -> Syn;
                            error            -> {error, File}
                        end.

clear_old_js() ->
    case file:list_dir(?JSDIR) of
        {error, _}  -> ok; % directory doesn't exist, that's ok
        {ok, Files} -> [ok = file:delete(?JSDIR ++ X) || X <- Files],
                       ok
    end.

has_js_dr() ->
    case file:list_dir(?JSDIR) of
        {error, _} -> false;
        {ok, _}    -> true
    end.
