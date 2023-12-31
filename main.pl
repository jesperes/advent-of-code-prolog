:- module(main, [main/1]).

:- use_module(library(prolog_stack)).
:- use_module(utils).
:- use_module(aocdata).
:- use_module(day01, [solve/1 as solve_day1 ] ).
:- use_module(day02, [solve/1 as solve_day2 ] ).
:- use_module(day03, [solve/1 as solve_day3 ] ).
:- use_module(day04, [solve/1 as solve_day4 ] ).
:- use_module(day05, [solve/1 as solve_day5 ] ).

main(_) :-
    format("Benchmarking...~n", []),
    findall(Result, benchmark_module(_, Result), Results),
    sort(Results, Sorted),
    format_results(Sorted).

format_results([]).
format_results([Result|Rest]) :-
    format_solution(Result),
    format_results(Rest).

%! Collect all the solution produced by the Module:solve/1 predicate.
call_aoc_puzzle(Module, Solutions) :-
    findall(Solution, Module:solve(Solution), Solutions).

benchmark_module(Module, {Module, Solution, TimeMsecs}) :-
    current_module(Module),
    module_property(Module, exports([solve/1])),
    benchmark(call_aoc_puzzle(Module, Solution), TimeMsecs).

format_solution({Module, Solution, TimeMsecs}) :-
    format("~w ~`.t ~g msecs ~30|~p\n", [Module, TimeMsecs, Solution]).

benchmark(Goal, TimeMsecs) :-
    statistics(walltime, [Start, _]),
    call(Goal),
    statistics(walltime, [Now, _]),
    TimeMsecs is Now - Start.

%% correct_answer(day01, [{part1,54391},{part2,54277}]).
%% correct_answer(Module, Other) :-
%%     swritef(S, "Incorrect answer for %w: %w", [Module, Other]),
%%     throw(S).
