:- module(utils, [file_line/2]).

file_line(File, Line) :-
    setup_call_cleanup(open(File, read, In),
                       stream_line(In, Line),
                       close(In)).

stream_line(In, Line) :-
    repeat,
    (  read_line_to_string(In, Line0),
       Line0 \== end_of_file
    -> Line0 = Line
    ;  !,
       fail
    ).

sum(Xs, Sum) :-
    sum(Xs, Sum, 0).

sum([], Sum, Sum).
sum([X|Xs], Sum, Acc) :-
    Acc0 is X + Acc,
    sum(Xs, Sum, Acc0).
