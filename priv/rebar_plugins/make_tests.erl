%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       This script makes common test suites
%%%
%%% @end
%%% Created :  15 Aug 2013 by gordonguthrie@backawinner.gg

-module(make_tests).

-export([
         make_tests/2
        ]).

-define(JSDIR,     "js/").
-define(TESTDIRS,  ["test/passing"]).

make_tests(_Config, AppFile) ->
    App = filename:basename(AppFile),
    case App of
        "luvviescript.app.src" ->
            code:add_patha("./ebin"),
            [ok = make_utils:make_tests(X) || X <- ?TESTDIRS],
            ok;
        _ ->
            ok
    end.
