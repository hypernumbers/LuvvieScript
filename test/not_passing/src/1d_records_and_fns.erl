-module('1c_records').

-record(bish, {
          bash = [],
          bosh = erk
         }).

-export([
         recordfn/1
        ]).

recordfn(#bish{bash = Bash}) ->
    Bash.
