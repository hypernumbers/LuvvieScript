-module(basic_comparisons).

-compile(export_all).

equals() ->
    4    == 5,
    4.0  == 4.0,
    a    == a,
    true == false,
    {bish, bosh} == {andy, pandy},
    [a, b]       == [a, b].

not_equals() ->
    4   /= 5,
    4.0 /= 4.0,
    a   /= a,
    true /= false,
    {bish, bosh} /= {andy, pandy},
    [a, b] /= [a, b].

less_than() ->
    4    =< 5,
    4.0  =< 4.0,
    a    =< a,
    true =< false,
    {bish, bosh} =< {andy, pandy},
    [a, b]       =< [a, b].

less() ->
    4    < 5,
    4.0  < 4.0,
    a    < a,
    true < false,
    {bish, bosh} < {andy, pandy},
    [a, b]       < [a, b].

greater_than() ->
    4    >= 5,
    4.0  >= 4.0,
    a    >= a,
    true >= false,
    {bish, bosh} >= {andy, pandy},
    [a, b]       >= [a, b].

greater() ->
    4    > 5,
    4.0  > 4.0,
    a    > a,
    true > false,
    {bish, bosh} > {andy, pandy},
    [a, b]       > [a, b].

exactly_eq() ->
    4    =:= 5,
    4.0  =:= 4.0,
    a    =:= a,
    true =:= false,
    {bish, bosh} =:= {andy, pandy},
    [a, b]       =:= [a, b].

exactly_neq() ->
    4    =/= 5,
    4.0  =/= 4.0,
    a    =/= a,
    true =/= false,
    {bish, bosh} =/= {andy, pandy},
    [a, b]       =/= [a, b].
