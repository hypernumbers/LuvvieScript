 This file contains the record definitions for LuvvieScript

```erlang
    -record(module, {
              name       = []    :: string() ,
              file       = []    :: string(),
              attributes = []    :: list(),
              export_all = false :: boolean(),
              exports    = []    :: list(),
              includes   = []    :: list(),
              records    = []    :: list()
             }).

    -record(function, {
              name      = [] :: string(),
              variables = [] :: list()
             }).

    -record(context, {
              module           = #module{}   :: #module{},
              current_function = #function{} :: #function{}
             }).
```
