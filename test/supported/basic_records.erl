-module(records).

-compile(export_all).

-record(bingo, {bish,
                bash = garle,
                bosh = running_dog()}).

luvvie_record_1() ->
    A = #bingo{}.

luvvie_record_2() ->
    A = #bingo{},
    A#bingo.bosh.

luvvie_record_3() ->
    #bingo.bash.

running_dog() ->
    "yowza".
