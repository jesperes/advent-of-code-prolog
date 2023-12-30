:- module(day04, [solve/1]).

:- use_module([utils,
               aocdata,
               library(ordsets)]).

solve({part1, P1}) :-
    findall(Score, card_value(Score), Scores),
    sum_card_values(Scores, 0, P1).

card_value(Score) :-
    input_line(4, Line),
    split_string(Line, " ", " :", ["Card", _CardId|Rest]),
    split_at(Rest, "|", Left, Right),
    list_to_ord_set(Left, LeftSet),
    list_to_ord_set(Right, RightSet),
    ord_intersect(LeftSet, RightSet, Intersection),
    length(Intersection, Len),
    Score = Len.

sum_card_values([], Sum, Sum).
sum_card_values([N|Rest], SumIn, SumOut) :-
    Sum0 is SumIn + (1 << (N - 1)),
    sum_card_values(Rest, Sum0, SumOut).
