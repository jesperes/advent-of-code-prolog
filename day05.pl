:- module(day05, [solve/1]).

:- use_module([utils,
               aocdata,
               library(pcre),
               library(clpfd)]).

%! apply_range_rules(+X, +Ranges, -Y, -Conds, -LS)
%
% Apply FD constraints for a list of [DstStart,SrcStart,Len] range
% mappings. X is the input intervals to be mapped represented as a
% single FD variable. Y is the output interval. Conds is unified with
% a list of reified 1/0 variables representing each of the
% conditions. This is used to be able to apply the default condition
% if none of the rules apply.
apply_range_rules(_, [], _, [], LS) :-
    empty_fdset(LS).
apply_range_rules(X, [Range|Ranges], Y, [Cond|Conds], LSOut) :-
    Range = [DstStart, SrcStart, Len],
    (X #>= SrcStart #/\ X #< (SrcStart + Len)) #<==> Cond,
    Cond #==> (Y #= (DstStart + (X - SrcStart))),
    apply_range_rules(X, Ranges, Y, Conds, LS0),
    Min is DstStart,
    Max is DstStart + Len - 1,
    fdset_interval(Int, Min, Max),
    fdset_union(Int, LS0, LSOut).

apply_map(X, Ranges, Y, LS) :-
    apply_range_rules(X, Ranges, Y, Conds, LS),
    sum(Conds, #=, NumConds),
    (NumConds #= 0) #==> (X #= Y).

apply_maps(X, [], X, LocSet) :-
    empty_fdset(LocSet).
apply_maps(X, [Map|Maps], Z, LocSet) :-
    apply_map(X, Map, Y, LS),
    apply_maps(Y, Maps, Z, LS0),
    fdset_union(LS, LS0, LocSet).

solve({part1, Location}) :-
    parse1(Seed, Maps),
    apply_maps(Seed, Maps, Location, LocSet),
    Location in_set LocSet,
    once(labeling([ff, min(Location)], [Seed, Location])).
solve({part2, Location}) :-
    parse2(Seed, Maps),
    apply_maps(Seed, Maps, Location, LocSet),
    Location in_set LocSet,
    once(labeling([ff, min(Location)], [Seed, Location])).

% Parser
parse1(Seed, Maps) :-
    parsed_input(SeedInts, Maps),
    list_to_fdset(SeedInts, FDSet),
    Seed in_set FDSet.

parse2(Seed, Maps) :-
    parsed_input(SeedInts, Maps),
    seedlist_to_fdset(SeedInts, FDSet),
    Seed in_set FDSet.

seedlist_to_fdset([], Seed) :-
    empty_fdset(Seed).
seedlist_to_fdset([Min, Len|Rest], FDSet) :-
    Max is Min + Len - 1,
    fdset_interval(Interval, Min, Max),
    seedlist_to_fdset(Rest, FDSetRest),
    fdset_union(Interval, FDSetRest, FDSet).

parsed_input(SeedInts, Maps) :-
    input(5, Input),
    re_split("\n\n", Input, [SeedStr|Sections]),
    split_string(SeedStr, " ", "", [_Header|SeedStrs]),
    maplist(number_string, SeedInts, SeedStrs),
    input_sections_to_maps(Sections, Maps).

input_sections_to_maps([], []).
input_sections_to_maps(["\n\n"|Rest], Maps) :-
    !,
    input_sections_to_maps(Rest, Maps).
input_sections_to_maps([Str|Rest], [Map|Maps]) :-
    split_string(Str, "\n", "", [_Header|Lines]),
    split_ranges(Lines, Map),
    input_sections_to_maps(Rest, Maps).

split_ranges([], []).
split_ranges([""|Rest], Maps) :-
    !,
    split_ranges(Rest, Maps).
split_ranges([Line|Rest], [Map|Maps]) :-
    split_string(Line, " ", "", L),
    maplist(number_string, Map, L),
    split_ranges(Rest, Maps).
