:- module(day03, [solve/1]).

:- use_module([utils,
               aocdata,
               library(pcre),
               library(assoc),
               library(pairs)]).

:- set_prolog_flag(re_compile, true).

solve(PartSol) :-
    input(3, Input),
    parse(Input, Symbols),
    do_solve(Symbols, PartSol).

do_solve(Symbols, {part1, P1}) :-
    assoc_to_values(Symbols, Values),
    flatten(Values, Flattened),
    pairs_values(Flattened, List),
    sum(List, P1).

do_solve(Symbols, {part2, P2}) :-
    assoc_to_list(Symbols, Pairs),
    foldl(sum_part2_sol, Pairs, 0, P2).

sum_part2_sol(_-[_-Gear1, _-Gear2], Sum, SumOut) :-
    SumOut is Gear1 * Gear2 + Sum,
    !.
sum_part2_sol(_, Sum, Sum).

%! Parse the input.
%
% The parsed output consist of an assoc list where the keys are
% AdjIdx-CharAt pairs with AdjIdx being the index of the adjacent
% symbol (*, /, etc), and CharAt is the corresponding symbol. The
% values are NumIdx-Num pairs, when NumIdx is the index of the number
% and Num is the actual number.
parse(Input, Symbols) :-
    line_width(Input, Width), !, % don't backtrack, only look at first line
    WNL is Width + 1,
    string_length(Input, InputLen),
    empty_assoc(SymbolsIn),
    re_foldl(matchpred, "(?<num>\\d+)"/r, Input,
             {Input, WNL, InputLen, SymbolsIn}, %% input accumulator
             {_, _, _, Symbols}, %% output accumulator
             []).

% Invoked for each number in the input. Folds over all digits in the
% number, inserting all symbols adjacent to a number into a set.
matchpred(Match, {Input, Width, InputLen, AccIn}, {Input, Width, InputLen, AccOut}) :-
    NumIdx-Len = Match.get(num),
    sub_string(Input, NumIdx, Len, _, NumStr),
    To is NumIdx + Len - 1,
    seq(NumIdx, To, Indexes),
    foldl(find_adj_symbols(NumIdx, NumStr, Input, Width, InputLen), Indexes, AccIn, AccOut).

find_adj_symbols(NumIdx, NumStr, Input, Width, InputLen, Index, AccIn, AccOut) :-
    findall(AdjIdx, adjacent_to(Index, Width, InputLen, AdjIdx), AdjacentIndexes),
    foldl(find_adj_symbols_inner(NumIdx, NumStr, Input), AdjacentIndexes, AccIn, AccOut).

find_adj_symbols_inner(NumIdx, NumStr, Input, AdjIdx, AccIn, AccOut) :-
    sub_string(Input, AdjIdx, 1, _, CharAt),
    % format("Checking ~p of number ~p at index ~p~n", [CharAt, NumStr, NumIdx]),
    is_adj_symbol(NumIdx, NumStr, AdjIdx, CharAt, AccIn, AccOut).

is_adj_symbol(_NumIdx, _NumStr, _AdjIdx, ".", Acc, Acc).
is_adj_symbol(_NumIdx, _NumStr, _AdjIdx, CharAt, Acc, Acc) :- char_type(CharAt, digit).
is_adj_symbol(_NumIdx, _NumStr, _AdjIdx, CharAt, Acc, Acc) :- char_type(CharAt, end_of_line).
is_adj_symbol(NumIdx, NumStr, AdjIdx, CharAt, AccIn, AccOut) :-
    number_string(Num, NumStr),
    Key = AdjIdx-CharAt,
    Elem = NumIdx-Num,
    (  get_assoc(Key, AccIn, Old)
    -> ord_add_element(Old, Elem, NewSet),
       put_assoc(Key, AccIn, NewSet, AccOut)
    ;  list_to_ord_set([Elem], Set),
       put_assoc(Key, AccIn, Set, AccOut)
    ).

% Backtracks over all (valid) indexes adjacent to Index
adjacent_to(Index, Width, Len, Adj) :-
    adjacent_to(Index, Width, Adj),
    Adj >= 0,
    Adj < Len.
adjacent_to(Index, _Width, Adj) :- Adj is Index - 1.
adjacent_to(Index, _Width, Adj) :- Adj is Index + 1.
adjacent_to(Index, Width, Adj)  :- Adj is Index - Width - 1.
adjacent_to(Index, Width, Adj)  :- Adj is Index - Width.
adjacent_to(Index, Width, Adj)  :- Adj is Index - Width + 1.
adjacent_to(Index, Width, Adj)  :- Adj is Index + Width - 1.
adjacent_to(Index, Width, Adj)  :- Adj is Index + Width.
adjacent_to(Index, Width, Adj)  :- Adj is Index + Width + 1.
