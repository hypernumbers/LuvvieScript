%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       This script compiles Erlang to Javascript
%%%
%%% @end
%%% Created :  15 Aug 2013 by gordonguthrie@backawinner.gg

-module(make_luvv).

-export([
         make_luvv/2
        ]).

-define(JSDIR,     "js/").
-define(TESTDIRS,  ["test/passing"]).

make_luvv(_Config, AppFile) ->
    App = filename:basename(AppFile),
    case App of
        "luvviescript.app.src" ->
            true = code:add_patha("./ebin"),
            [ok = make_utils:compile(X, debug) || X <- ?TESTDIRS],
            ok;
        _ ->
            ok
    end.

