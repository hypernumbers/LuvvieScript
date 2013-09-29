 This file contains the record definitions for LuvvieScript

```erlang
    -record(module, {
              name              :: string() ,
              file              :: string(),
              attributes        :: list(),
              export_all= flase :: boolean(),
              exports           :: list(),
              includes          :: list(),
              records           :: list(),
              contents          :: list()
             }).

    -record(function, {
              name          :: string(),
              arity         :: integer(),
              line_no,
              contents = [] :: list()
             }).

    -record(clause, {
              params   :: list(),
              guards   :: list(),
              line_no  :: list(),
              contents :: list()
             }).

    -record(statement, {
              contents = [] :: list()
             }).
```
