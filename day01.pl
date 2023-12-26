:- module(day01, [solve/1,
                  first_digit_with_words/2,
                  last_digit_with_words/2
                 ]).

filename("input1.txt").

solve({part1, Sum}) :-
    findall(Value, calibration_values(Value, part1), Values),
    utils:sum(Values, Sum).

solve({part2, Sum}) :-
    findall(Value, calibration_values(Value, part2), Values),
    utils:sum(Values, Sum).

%% Calibration values part 1
calibration_values(Value, Part) :-
    filename(Filename),
    utils:file_line(Filename, Line),
    string_to_list(Line, List),
    first_and_last_digits(First, Last, List, Part),
    number_codes(Value, [First, Last]).

first_and_last_digits(First, Last, List, part1) :-
    first_digit(List, First),
    last_digit(List, Last).

first_and_last_digits(First, Last, List, part2) :-
    first_digit_with_words(List, First),
    last_digit_with_words(List, Last).

%% Digit is the first digit in String
first_digit([Digit|_Rest], Digit) :-
    char_type(Digit, digit),
    !.
first_digit([_|Rest], Digit) :-
    first_digit(Rest, Digit).

%% Digit is the last digit in String
last_digit([Digit|Rest], Digit) :-
    char_type(Digit, digit),
    no_digits(Rest).
last_digit([_|Rest], Digit) :-
    last_digit(Rest, Digit).

%% Succeed if the given list contains no digits.
no_digits([]).
no_digits([NonDigit|Rest]) :-
    char_type(NonDigit, alpha),
    no_digits(Rest).

first_digit_with_words(List, Digit) :-
    is_digit_prefix(List, Digit),
    !.
first_digit_with_words([_|List], Digit) :-
    first_digit_with_words(List, Digit).

%% Succeed if List begins with something that is either a digit or a
%% digit-word.
is_digit_prefix([Digit|_List], Digit) :-
    char_type(Digit, digit),
    !.
is_digit_prefix([122, 101, 114, 111|_List], 48) :- !.      %% zero
is_digit_prefix([111, 110, 101|_List], 49) :- !.           %% one
is_digit_prefix([116, 119, 111|_List], 50) :- !.           %% two
is_digit_prefix([116, 104, 114, 101, 101|_List], 51) :- !. %% three
is_digit_prefix([102, 111, 117, 114|_List], 52) :- !.      %% four
is_digit_prefix([102, 105, 118, 101|_List], 53) :- !.      %% five
is_digit_prefix([115, 105, 120|_List], 54) :- !.           %% six
is_digit_prefix([115, 101, 118, 101, 110|_List], 55) :- !. %% seven
is_digit_prefix([101, 105, 103, 104, 116|_List], 56) :- !. %% eight
is_digit_prefix([110, 105, 110, 101|_List], 57) :- !.      %% nine
is_digit_prefix([_|List], Digit) :-
    is_digit_prefix(List, Digit).


last_digit_with_words(List, Digit) :-
    reverse(List, ListRev),
    last_digit_with_words_rev(ListRev, Digit).

last_digit_with_words_rev(ListRev, Digit) :-
    is_digit_prefix_rev(ListRev, Digit),
    !.
last_digit_with_words_rev([_|ListRev], Digit) :-
    last_digit_with_words_rev(ListRev, Digit).


%% Same as is_digit_prefix/2, but operates on a reversed string.
is_digit_prefix_rev([Digit|_List], Digit) :-
    char_type(Digit, digit),
    !.
is_digit_prefix_rev([111, 114, 101, 122|_List], 48) :- !.      %% orez
is_digit_prefix_rev([101, 110, 111|_List], 49) :- !.           %% eno
is_digit_prefix_rev([111, 119, 116|_List], 50) :- !.           %% owt
is_digit_prefix_rev([101, 101, 114, 104, 116|_List], 51) :- !. %% eerht
is_digit_prefix_rev([114, 117, 111, 102|_List], 52) :- !.      %% ruof
is_digit_prefix_rev([101, 118, 105, 102|_List], 53) :- !.      %% evif
is_digit_prefix_rev([120, 105, 115|_List], 54) :- !.           %% xis
is_digit_prefix_rev([110, 101, 118, 101, 115|_List], 55) :- !. %% neves
is_digit_prefix_rev([116, 104, 103, 105, 101|_List], 56) :- !. %% thgie
is_digit_prefix_rev([101, 110, 105, 110|_List], 57) :- !.      %% enin
is_digit_prefix_rev([_|List], Digit) :-
    is_digit_prefix_rev(List, Digit).
