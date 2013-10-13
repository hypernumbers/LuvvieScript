   @author    Gordon Guthrie
   @copyright (C) 2013, Gordon Guthrie
   @doc       This module converts the (slightly amended)
              Erlang AST to the javascript one

   @end
   Created : 17 Aug 2013 by gordon@vixo.com
```erlang
    -module(to_js).

    -export([
             conv/2
            ]).

    -include("luvviescript.hrl").

    conv(Node, Context) ->
        io:format("Node is:~n-~p~n Context is:~n-~p~n", [Node, Context]),
        Node.
```
