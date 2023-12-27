:- module(day03, [solve/1]).

:- use_module(utils).
:- use_module(aocdata).
:- use_module(library(pcre)).
%% :- use_module(library(hashtable)).

:- set_prolog_flag(re_compile, true).

solve({part1, Parsed}) :-
    input(3, Input),
    parse(Input, Parsed).
solve({part2, 0}).

parse(Input, Symbols) :-
    re_foldl(matchpred, "(?<num>\\d+)"/r, Input,
             {Input, symbols{}}, %% input accumulator
             {_, Symbols}, %% output accumulator
             []).

matchpred(Match, {Input, AccIn}, {Input, AccOut}) :-
    Index-Len = Match.get(num),
    find_adj_symbols(Index, Len, Width, Input, AccIn, AccOut).
