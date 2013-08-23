-module(basic_including).

-compile(export_all).

-include("included.hrl").

including() ->


            #brannigan{}.

location() ->      dingo().

complex_clauses() ->
    A=1,
    B = case A of
            1 ->  "stick the return up here";
        3 ->
                "stick it down here"
        end, C = 7,
    Z1 = [X + 2
     ||
        X <- [A, B, C]],
    Z2 = [X + 2 || X <- [A, B, C]],
    {Z1, Z2}.



