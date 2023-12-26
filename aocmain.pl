:- module(aocmain, [aocmain/1]).

:- use_module(library(prolog_stack)).
:- use_module(utils).
:- use_module(aocdata).
:- use_module(day01, [solve/1 as solve_day1 ] ).
:- use_module(day02, [solve/1 as solve_day2 ] ).

repeat(100).

aocmain(_) :-
    format("Benchmarking...~n", []),
    findall(Day, benchmark_module(Day), _Result),
    format("Done~n", []).

%! Collect all the solution produced by the Module:solve/1 predicate.
call_aoc_puzzle(Module, Solutions) :-
    findall(Solution, Module:solve(Solution), Solutions).

benchmark_module(Module) :-
    current_module(Module),
    module_property(Module, exports([solve/1])),
    benchmark(call_aoc_puzzle(Module, Solution), TimeMsecs),
    format_solution(Module, Solution, TimeMsecs).

format_solution(Module, Solution, TimeMsecs) :-
    format("~w: ~w msecs (~w)~n", [Module, TimeMsecs, Solution]).

benchmark(Goal, TimeMsecs) :-
    statistics(walltime, _),
    repeat(Repeat),
    call_repeat(Repeat, Goal),
    statistics(walltime, [_, TotalTimeMsecs]),
    TimeMsecs is TotalTimeMsecs / Repeat.

call_repeat(0, _Goal).
call_repeat(N, Goal) :-
    N >= 0,
    call(Goal),
    N0 is N - 1,
    call_repeat(N0, Goal).

%% correct_answer(day01, [{part1,54391},{part2,54277}]).
%% correct_answer(Module, Other) :-
%%     swritef(S, "Incorrect answer for %w: %w", [Module, Other]),
%%     throw(S).
