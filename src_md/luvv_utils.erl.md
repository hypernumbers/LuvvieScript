-------------------------------------------------------------------
 @author    Gordon Guthrie
 @copyright (C) 2014, Gordon Guthrie
 @doc       This is a utility file that is available client and
            server side where it performs slightly different roles

 @end
 Created :  2 Mar 2014 by gordon@vixo.com
-------------------------------------------------------------------
```erlang
    -module(luvv_utils).

    -export([
             external_format/1
            ]).

    external_format(X) when is_integer(X) -> io_lib:format("{\"int\": ~p}",   [X]);
    external_format(X) when is_float(X)   -> io_lib:format("{\"float\": ~p}", [X]);
    external_format(X) when is_atom(X)    -> io_lib:format("{\"atom\": ~p}",  [X]);
    external_format(X) when is_tuple(X)   -> io_lib:format("{\"tuple\": ~p}", [X]);
    external_format(X) when is_list(X)    -> io_lib:format("{\"list\": ~p}",  [X]).
```
