:- module(aocdata,
          [input/2,
           input_line/2
          ]).

:- use_module(utils).
:- use_module(library(readutil)).
:- use_module(library(http/http_client)).

cache_filename(Day, Filename) :-
    swritef(Str, "~/.cache/aoc-data/2023/input%w.txt", [Day]),
    expand_file_name(Str, [Filename|_]).

session_cookie(Cookie) :-
    expand_file_name("~/.adventofcode.session", [Filename|_]),
    readutil:read_file_to_string(Filename, CookieRaw, []),
    normalize_space(string(Cookie), CookieRaw).

%! input_file(+Day, -Filename)
%
% Download the input for the given day, and unify Filename with the
% locally cached filename.
input_file(Day, Filename) :-
    cache_filename(Day, Filename),
    access_file(Filename, read),
    !.
input_file(Day, Filename) :-
    format("Downloading input for day ~w~n", [Day]),
    session_cookie(Cookie),
    swritef(URL, "https://adventofcode.com/%w/day/%w/input", [2023, Day]),
    swritef(CookieHeader, "session=%w", [Cookie]),
    http_get(URL, Input, [request_header('cookie'=CookieHeader)]),
    cache_filename(Day, Filename),
    format("Cached input file in ~w~n", [Filename]),
    write_to_file(Input, Filename).

%! input(+Day, -Input)
%
% Unified Input with the input data for the given day.
input(Day, Input) :-
    input_file(Day, Filename),
    readutil:read_file_to_string(Filename, Input, []).

%! input_line(+Day, -Line)
%
% Unifies Line with lines in the input data for the given day, such
% that backtracking with yield all input lines in order.
input_line(Day, Line) :-
    input_file(Day, Filename),
    setup_call_cleanup(open(Filename, read, In),
                       stream_line(In, Line),
                       close(In)).

stream_line(In, Line) :-
    repeat,
    (  read_line_to_string(In, Line0),
       Line0 \== end_of_file
    -> Line0 = Line
    ;  !,
       fail
    ).
