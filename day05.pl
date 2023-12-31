:- module(day05, [
                  test_constraints/1
                 ]).

:- use_module([utils,
               aocdata,
               library(clpfd)]).

range_rule(VarIn, VarOut, DestStart, SourceStart, Len, Rule) :-
    (VarIn #>= SourceStart #/\ VarIn #< (SourceStart + Len)) #<==> Rule,
    Rule #==> (VarOut #= (DestStart + (VarIn - SourceStart))).

default(VarIn, VarOut, Rule) :-
    (VarIn #= VarOut) #<==> Rule.

%% apply_range_rule([], _, [], []).
%% apply_range_rule([X|Xs], Rule, [Y|Ys], [Cond|Conds]) :-
%%     Rule = [DstStart, SrcStart, Len],
%%     (X #>= SrcStart #/\ X #< (SrcStart + Len)) #<==> Cond,
%%     Cond #==> (Y #= (DstStart + (X - SourceStart))).
%%     apply_range_rule(Xs, Rule, Ys, Conds).

%% apply_map(Vars, [], Vars).
%% apply_map(VarsIn, [Range|Rest], VarsOut) :-
%%     apply_range_rule(VarsIn, Range, VarsOut, Conds),



test_constraints(Result) :-

    X1 in 0..99,
    Y1 in 0..99,

    %% apply_map([X],
    %%           [[50, 98, 2],
    %%            [52, 50, 48]],
    %%           [Y]).

    % seed-to-soil
    range_rule(X1, Y1, 50, 98, 2, RuleY1),
    range_rule(X1, Y1, 52, 50, 48, RuleY2),
    default(X1, Y1, RuleY3),
    (RuleY1 #= 0) #/\ (RuleY2 #= 0) #==> RuleY3 #= 1,

    indomain(X1),
    indomain(Y1),

    Result=X1-Y1.
