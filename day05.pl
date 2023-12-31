:- module(day05, [
                  test_constraints/1
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

test_constraints(Y) :-

    (X #= 79) #\/ (X #= 14) #\/ (X #= 55) #\/ (X #= 13),
    Y in 0..99,

    apply_maps(X,

               % seed-to-soil
               [[[50, 98, 2],
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
                 [56, 93, 4]]],
              Y),

    labeling([min(Y)], [X, Y]).
