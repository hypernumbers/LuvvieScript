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
-define(TESTDIRS,  ["test/passing", "test/not_passing"]).

make_tests(_A, _B) ->
    code:add_patha("./ebin"),
    [ok = make_utils:make_tests(X) || X <- ?TESTDIRS],
    ok.

