:- module(day01, [solve/1,
                  calibration_values/2]).

solve({part1, Sum}) :-
    findall(Value, calibration_values("input1.txt", Value), Values),
    utils:sum(Values, Sum).

calibration_values(Filename, Value) :-
    utils:file_line(Filename, Line),
    first_digit(Line, First),
    last_digit(Line, Last),
    number_codes(Value, [First, Last]).

%% Digit is the first digit in String
first_digit(String, Digit) :-
    string(String),
    string_to_list(String, List),
    first_digit(List, DigitCode),
    char_code(Digit, DigitCode).

first_digit([Digit|_Rest], Digit) :-
    char_type(Digit, digit),
    !.
first_digit([_|Rest], Digit) :-
    first_digit(Rest, Digit).

%% Digit is the last digit in String
last_digit(String, Digit) :-
    string(String),
    string_to_list(String, List),
    last_digit(List, DigitCode),
    char_code(Digit, DigitCode).

last_digit([Digit|Rest], Digit) :-
    char_type(Digit, digit),
    no_digits(Rest).
last_digit([_|Rest], Digit) :-
    last_digit(Rest, Digit).

no_digits([]).
no_digits([NonDigit|Rest]) :-
    char_type(NonDigit, alpha),
    no_digits(Rest).
