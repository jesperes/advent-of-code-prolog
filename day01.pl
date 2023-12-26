:- module(day01, [solve/1]).

solve({part1, Sum}) :-
    findall(Value, calibration_values(Value, part1), Values),
    utils:sum(Values, Sum).

solve({part2, Sum}) :-
    findall(Value, calibration_values(Value, part2), Values),
    utils:sum(Values, Sum).

calibration_values(Value, Part) :-

    filename(Filename),
    utils:file_line(Filename, Line),
    string_to_list(Line, List),
    first_and_last_digits(First, Last, List, Part),
    number_codes(Value, [First, Last]).

first_and_last_digits(First, Last, List, Part) :-
    first_digit(List, First, Part),
    last_digit(List, Last, Part).

%% Digit is the first digit in String
first_digit(List, Digit, Part) :-
    is_digit_prefix(Part, List, Digit),
    !.
first_digit([_|Rest], Digit, Part) :-
    first_digit(Rest, Digit, Part).

%% Digit is the last digit in String
last_digit(List, Digit, Part) :-
    reverse(List, ListRev),
    last_digit_rev(ListRev, Digit, Part).

last_digit_rev(ListRev, Digit, Part) :-
    is_digit_prefix(Part, ListRev, Digit),
    !.
last_digit_rev([_|ListRev], Digit, Part) :-
    last_digit_rev(ListRev, Digit, Part).

is_digit_prefix(_, [Digit|_List], Digit) :-
    char_type(Digit, digit),
    !.

is_digit_prefix(part2, [122, 101, 114, 111|_List], 48) :- !.      %% zero
is_digit_prefix(part2, [111, 110, 101|_List], 49) :- !.           %% one
is_digit_prefix(part2, [116, 119, 111|_List], 50) :- !.           %% two
is_digit_prefix(part2, [116, 104, 114, 101, 101|_List], 51) :- !. %% three
is_digit_prefix(part2, [102, 111, 117, 114|_List], 52) :- !.      %% four
is_digit_prefix(part2, [102, 105, 118, 101|_List], 53) :- !.      %% five
is_digit_prefix(part2, [115, 105, 120|_List], 54) :- !.           %% six
is_digit_prefix(part2, [115, 101, 118, 101, 110|_List], 55) :- !. %% seven
is_digit_prefix(part2, [101, 105, 103, 104, 116|_List], 56) :- !. %% eight
is_digit_prefix(part2, [110, 105, 110, 101|_List], 57) :- !.      %% nine

is_digit_prefix(part2, [111, 114, 101, 122|_List], 48) :- !.      %% orez
is_digit_prefix(part2, [101, 110, 111|_List], 49) :- !.           %% eno
is_digit_prefix(part2, [111, 119, 116|_List], 50) :- !.           %% owt
is_digit_prefix(part2, [101, 101, 114, 104, 116|_List], 51) :- !. %% eerht
is_digit_prefix(part2, [114, 117, 111, 102|_List], 52) :- !.      %% ruof
is_digit_prefix(part2, [101, 118, 105, 102|_List], 53) :- !.      %% evif
is_digit_prefix(part2, [120, 105, 115|_List], 54) :- !.           %% xis
is_digit_prefix(part2, [110, 101, 118, 101, 115|_List], 55) :- !. %% neves
is_digit_prefix(part2, [116, 104, 103, 105, 101|_List], 56) :- !. %% thgie
is_digit_prefix(part2, [101, 110, 105, 110|_List], 57) :- !.      %% enin

is_digit_prefix(Part, [_|List], Digit) :-
    is_digit_prefix(Part, List, Digit).
