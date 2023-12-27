:- module(utils, [sum/2,
                  line_width/2,
                  next_integer/1,
                  next_integer/2,
                  seq/3
                 ]).

%! Unify Width with the width of lines in Str.
%
% Backtracks over all line widths in Str.
line_width(Str, Width) :-
    sub_string(Str, Width, _, _, "\n").

sum(Xs, Sum) :-
    sum(Xs, Sum, 0).

sum([], Sum, Sum).
sum([X|Xs], Sum, Acc) :-
    Acc0 is X + Acc,
    sum(Xs, Sum, Acc0).


next_integer(I) :-
    next_integer(0, I).

next_integer(I, I).
next_integer(I, J) :-
    I2 is I + 1,
    next_integer(I2, J).

seq(Low, High, List) :-
    findall(X, between(Low, High, X), List).
