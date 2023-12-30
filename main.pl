:- module(main, [main/1]).

:- use_module(library(prolog_stack)).
:- use_module(utils).
:- use_module(aocdata).
:- use_module(day01, [solve/1 as solve_day1 ] ).
:- use_module(day02, [solve/1 as solve_day2 ] ).
:- use_module(day03, [solve/1 as solve_day3 ] ).
:- use_module(day04, [solve/1 as solve_day4 ] ).

repeat(100).
max_msecs(50).

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

benchmark_module(Module, {Module, Solution, Iters, TimeMsecs}) :-
    current_module(Module),
    module_property(Module, exports([solve/1])),
    benchmark(call_aoc_puzzle(Module, Solution), Iters, TimeMsecs).

format_solution({Module, Solution, Iters, TimeMsecs}) :-
    writef("%w: %w msecs (%p) (%p iters)\n", [Module, TimeMsecs, Solution, Iters]).

benchmark(Goal, Iters, TimeMsecs) :-
    statistics(walltime, [Start, _]),
    repeat(Repeat),
    max_msecs(MaxMsecs),
    call_repeat(Repeat, Start, MaxMsecs, Goal, 0, Iters),
    statistics(walltime, [Now, _]),
    TotalTimeMsecs is Now - Start,
    TimeMsecs is TotalTimeMsecs / Iters.

call_repeat(0, _, _, _Goal, Iters, Iters).
call_repeat(_, StartMsecs, MaxMsecs, _Goal, Iters, Iters) :-
    statistics(walltime, [Now, _]),
    Now - StartMsecs > MaxMsecs,
    !.
call_repeat(N, StartMsecs, MaxMsecs, Goal, Iters, NumIters) :-
    N >= 0,
    call(Goal),
    N0 is N - 1,
    NextIters is Iters + 1,
    call_repeat(N0, StartMsecs, MaxMsecs, Goal, NextIters, NumIters).

%% correct_answer(day01, [{part1,54391},{part2,54277}]).
%% correct_answer(Module, Other) :-
%%     swritef(S, "Incorrect answer for %w: %w", [Module, Other]),
%%     throw(S).
