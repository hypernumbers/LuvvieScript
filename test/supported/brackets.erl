-module(brackets).

-compile(export_all).
%% yargle
brackets() ->
    B = 1,
    C = 33,
    ((1 +       B) * C / 88). % donught
