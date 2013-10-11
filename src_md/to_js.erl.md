   @author    Gordon Guthrie
   @copyright (C) 2013, Gordon Guthrie
   @doc       This module converts the (slightly amended)
              Erlang AST to the javascript one

   @end
   Created : 17 Aug 2013 by gordon@vixo.com
```erlang
    -module(to_js).

    -export([
             conv/1
            ]).

    conv(Node) ->
        %% io:format("Node is ~p~n", [Node]),
        Node.
```
