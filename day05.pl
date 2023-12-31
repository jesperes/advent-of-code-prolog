:- module(day05, [ex1/1,
                  ex2/1
                 ]).

:- use_module([utils,
               aocdata,
               library(clpfd)]).

apply_range_rules(_, [], _, []).
apply_range_rules(X, [Range|Ranges], Y, [Cond|Conds]) :-
    Range = [DstStart, SrcStart, Len],
    (X #>= SrcStart #/\ X #< (SrcStart + Len)) #<==> Cond,
    Cond #==> (Y #= (DstStart + (X - SrcStart))),
    apply_range_rules(X, Ranges, Y, Conds).

apply_map(X, Ranges, Y) :-
    apply_range_rules(X, Ranges, Y, Conds),
    sum(Conds, #=, NumConds),
    (NumConds #= 0) #==> (X #= Y).

apply_maps(X, [], X).
apply_maps(X, [Map|Maps], Z) :-
    apply_map(X, Map, Y),
    apply_maps(Y, Maps, Z).

example_data([[[50, 98, 2],
               [52, 50, 48]],

              % soil-to-fertilizer
              [[0, 15, 37],
               [37, 52, 2],
               [39, 0, 15]],

              % fertilizer-to-water
              [[49, 53, 8],
               [0, 11, 42],
               [42, 0, 7],
               [57, 7, 4]],

              % water-to-light
              [[88, 18, 7],
               [18, 25, 70]],

              % light-to-temperature
              [[45, 77, 23],
               [81, 45, 19],
               [68, 64, 13]],

              % temperature-to-humidity
              [[0, 69, 1],
               [1, 0, 69]],

              % humidity-to-location
              [[60, 56, 37],
               [56, 93, 4]]]).

ex1(Location) :-
    (Seed #= 79) #\/ (Seed #= 14) #\/ (Seed #= 55) #\/ (Seed #= 13),
    Location in 0..99,

    example_data(Maps),
    apply_maps(Seed, Maps, Location),

    % Find the smallest location possible
    once(labeling([min(Location)], [Seed, Location])).

ex2(Location) :-
    Seed1 in 79..92,
    Seed2 in 55..67,
    Location in 0..99,
    Location1 in 0..99,
    Location2 in 0..99,

    example_data(Maps),
    apply_maps(Seed1, Maps, Location1),
    apply_maps(Seed2, Maps, Location2),

    (Location1 #< Location2) #==> (Location #= Location1),
    (Location1 #>= Location2) #==> (Location #= Location2),

    % Find the smallest location possible
    labeling([min(Location)], [Seed1, Seed2, Location1, Location2, Location]).
