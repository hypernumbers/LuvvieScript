%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       Sets up luvviescript the first time
%%%
%%% @end
%%% Created :  15 Aug 2013 by gordonguthrie@backawinner.gg

-module(first_time).

-export([
         first_time/2
        ]).

first_time(_Config, AppFile) ->
    App = filename:basename(AppFile),
    case App of
        "undefined" -> ok = first_t();
        _           -> ok
    end.

first_t() ->
    io:format("Creating the luvviescript.app file that rebar needs~n"),
    ok = filelib:ensure_dir("ebin/"),
    {ok, _} = file:copy("priv/first_time/luvviescript.app", "ebin/luvviescript.app"),
    ok.
