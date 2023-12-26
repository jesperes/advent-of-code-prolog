:- module(aocdata, [input/2,
                    input_lines/2,
                    session_cookie/1
                   ]).

:- use_module(library(readutil)).
:- use_module(library(http/http_client)).

cache_filename(Day, Filename) :-
    swritef(Str, "~/.cache/aoc-data/2023/input%w.txt", [Day]),
    expand_file_name(Str, [Filename|_]).

session_cookie(Cookie) :-
    expand_file_name("~/.adventofcode.session", [Filename|_]),
    readutil:read_file_to_string(Filename, CookieRaw, []),
    normalize_space(string(Cookie), CookieRaw).

input(Day, Input) :-
    cache_filename(Day, Filename),
    access_file(Filename, read),
    format("Using cached input for day ~w~n", [Day]),
    readutil:read_file_to_string(Filename, Input, []),
    !.
input(Day, Input) :-
    format("Downloading input for day ~w~n", [Day]),
    session_cookie(Cookie),
    swritef(URL, "https://adventofcode.com/%w/day/%w/input", [2023, Day]),
    swritef(CookieHeader, "session=%w", [Cookie]),
    http_get(URL, Input, [request_header('cookie'=CookieHeader)]),
    cache_filename(Day, Filename),
    format("Cached input file in ~w~n", [Filename]),
    utils:write_to_file(Input, Filename).

input_lines(Day, Lines) :-
    input(Day, Input),
    split_string(Input, "\n", "\n", Lines).
