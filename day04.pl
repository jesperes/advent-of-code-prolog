:- module(day04, [solve/1]).

:- use_module([utils,
               aocdata,
               library(ordsets)]).

solve({part1, P1}) :-
    findall(Score, card_value(Score), Scores),
    sum_card_values(Scores, 0, P1).

solve({part2, P1}) :-
    input_lines(4, Lines),
    empty_assoc(Lin),
    foldl(count_cards, Lines, Lin, Lout),
    assoc_to_values(Lout, Values),
    sum(Values, S1),
    length(Lines, NumCards),
    P1 is S1 + NumCards.

% Part 1
card_value(Score) :-
    input_line(4, Line),
    split_card_line(Line, NumMatching, _CardId),
    Score = NumMatching.

sum_card_values([], Sum, Sum).
sum_card_values([N|Rest], SumIn, SumOut) :-
    Sum0 is SumIn + (1 << (N - 1)),
    sum_card_values(Rest, Sum0, SumOut).

% Part 2
count_cards(Line, Lin, Lout) :-
    split_card_line(Line, NumMatching, Card),
    From is Card + 1,
    To is Card + NumMatching,
    seq(From, To, Cards),
    foldl(count_one_card(Card), Cards, Lin, Lout).

count_one_card(Card, I, Lin, Lout) :-
    get_assoc_default(Card, Lin, CardVal0, 0),
    CardVal1 is CardVal0 + 1,
    sum_assoc(I, CardVal1, Lin, Lout).

sum_assoc(Key, Val, AssocIn, AssocOut) :-
    get_assoc_default(Key, AssocIn, Old, 0),
    Sum is Old + Val,
    put_assoc(Key, AssocIn, Sum, AssocOut).

% -- Helpers --

%! Split a line in the input.
%
% Unify NumMatching with the number of matching cards, and CardId with
% the id of the card. We don't need any other info.
split_card_line(Line, NumMatching, CardId) :-
    split_string(Line, " ", " :", ["Card", CardIdStr|Rest]),
    number_string(CardId, CardIdStr),
    split_at(Rest, "|", Left, Right),
    list_to_ord_set(Left, LeftSet),
    list_to_ord_set(Right, RightSet),
    ord_intersect(LeftSet, RightSet, Intersection),
    length(Intersection, NumMatching).
