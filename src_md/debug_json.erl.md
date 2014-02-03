    @author    Gordon Guthrie
    @copyright (C) 2014, Gordon Guthrie
    @doc       sometimes you need to dig into the json you
               have created - this module can help debug it
               mebbies, YMMV

    @end
    Created : 10th January 2014 by gordon@vixo.com
```erlang
    -module(debug_json).

    -export([
            debug/1
           ]).

    debug(Json) ->
        dbg(Json, []),
        ok.

    dbg([], Acc) ->
        Acc;
    dbg({obj, List}, Acc) ->
        dbg(List, Acc);
    dbg(List, Acc) when is_list(List) ->
        %% io:format("List is ~p~n", [List]),
        {Keys, _Vals} = lists:unzip(List),
        %% io:format("Keys are ~p~n-in ~p~n", [Keys, lists:reverse(Acc)]),
        io:format("Keys are ~p~n", [Keys]),
        NewA = case lists:keyfind("type", 1, List) of
                   {"type", Type} ->
                       [Type | Acc];
                   _              ->
                       io:format("No type!~n"),
                       Acc
               end,
        TraverseFn = fun({"body", X}, FnAcc) ->
                             dbg(X, ["body" | FnAcc]);
                        ({"expression", false}, FnAcc) ->
                             FnAcc;
                        ({"expression", X}, FnAcc) ->
                             dbg(X, ["expression" | FnAcc]);
                        %% ({"callee", X}, FnAcc) ->
                        %%     dbg(X, ["callee" | FnAcc]);
                        %% ({"name", X}, FnAcc) ->
                        %%     dbg(X, ["name" | FnAcc]);
                        ({"left", X}, FnAcc) ->
                             dbg(X, ["left" | FnAcc]);
                        ({"right", X}, FnAcc) ->
                             dbg(X, ["right" | FnAcc]);
                        ({"cases", X}, FnAcc) ->
                             dbg(X, ["cases" | FnAcc]);
                        ({"consequent", X}, FnAcc) ->
                             dbg(X, ["consequent" | FnAcc]);
                        ({"discriminant", X}, FnAcc) ->
                             dbg(X, ["discriminant" | FnAcc]);
                        ({"loc", _X}, FnAcc) ->
                                FnAcc;
                        ({"type", _X}, FnAcc) ->
                                FnAcc;
                        ({obj, X}, FnAcc) ->
                             dbg(X, FnAcc);
                        (X, FnAcc) ->
                             io:format("Ignoring ~p~n", [X]),
                             FnAcc
                     end,
        lists:foldl(TraverseFn, NewA, List).
```
