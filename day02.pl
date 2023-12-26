:- module(day02, [solve/1]).

:- use_module(utils).

solve({part1, P1}) :-
    findall(GameId, valid_game_id(GameId), GameIds),
    sum(GameIds, P1).
solve({part2, P2}) :-
    findall(Power, game_power(Power), Powers),
    sum(Powers, P2).

game(GameId, Draws) :-
    aocdata:input_line(2, Line),
    split_string(Line, " ", ":; ,", ["Game", GameIdStr|Draws]),
    number_string(GameId, GameIdStr).

%% Part 1
valid_game_id(ValidGameId) :-
    game(GameId, Draws),
    valid_game(Draws, GameId, ValidGameId).

valid_game([], GameId, GameId).
valid_game([Cubes, Color|Rest], GameId, ValidGameId) :-
    number_string(N, Cubes),
    num_cubes(Color, Max),
    N =< Max,
    valid_game(Rest, GameId, ValidGameId).

%% Part 2
game_power(Power) :-
    game(_, Draws),
    game_power(Draws, {0, 0, 0}, Power).

game_power([], {R, G, B}, Power) :-
    Power is R * G * B.
game_power([Cubes, Color|Rest], RGB, Power) :-
    number_string(N, Cubes),
    min_rgb(N, Color, RGB, MaxRGB),
    game_power(Rest, MaxRGB, Power).

min_rgb(N, "red", {R, G, B}, {N, G, B}) :- N > R, !.
min_rgb(N, "green", {R, G, B}, {R, N, B}) :- N > G, !.
min_rgb(N, "blue", {R, G, B}, {R, G, N}) :- N > B, !.
min_rgb(_, _, RGB, RGB).

num_cubes("red", 12).
num_cubes("green", 13).
num_cubes("blue", 14).
