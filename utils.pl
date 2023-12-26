:- module(utils, [sum/2]).

sum(Xs, Sum) :-
    sum(Xs, Sum, 0).

sum([], Sum, Sum).
sum([X|Xs], Sum, Acc) :-
    Acc0 is X + Acc,
    sum(Xs, Sum, Acc0).
