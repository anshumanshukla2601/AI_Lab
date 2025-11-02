% Define GCD
solve_gcd(A, 0, A) :- !.
solve_gcd(A, B, GCD) :-
    R is A mod B,
    solve_gcd(B, R, GCD).

% Check if goal reached
reached_goal(Z, CurX, CurY) :-
    CurX =:= Z ; CurY =:= Z.

% Fill jug X
fill_x(X, _, X).

% Fill jug Y
fill_y(Y, _, Y).

% Empty jug X
empty_x(_, 0).

% Empty jug Y
empty_y(_, 0).

% Pour from X to Y
pour_x_to_y(X, Y, CurX, CurY, NewX, NewY) :-
    Transfer is min(CurX, Y - CurY),
    NewX is CurX - Transfer,
    NewY is CurY + Transfer.

% Main water jug solver
water_jug(X, Y, Z) :-
    (   Z > max(X, Y)
    ->  write('Goal not achievable with given jug sizes.'), nl
    ;   solve_gcd(X, Y, GCD),
        (   0 =\= Z mod GCD
        ->  write('Goal not achievable with given jug sizes.'), nl
        ;   CurX = 0,
            CurY = 0,
            format('Initial state: (~w, ~w)~n', [CurX, CurY]),
            water_jug_loop(X, Y, Z, CurX, CurY)
        )
    ).

% Recursive loop until goal is reached
water_jug_loop(_, _, Z, CurX, CurY) :-
    reached_goal(Z, CurX, CurY),
    format('Goal reached! Final state: (~w, ~w)~n', [CurX, CurY]), !.

water_jug_loop(X, Y, Z, CurX, CurY) :-
    (   CurX =:= 0
    ->  fill_x(X, CurX, NewX),
        format('Fill X: (~w, ~w)~n', [NewX, CurY]),
        water_jug_loop(X, Y, Z, NewX, CurY)
    ;   CurX > 0, CurY < Y
    ->  pour_x_to_y(X, Y, CurX, CurY, NewX, NewY),
        format('Pour X->Y: (~w, ~w)~n', [NewX, NewY]),
        water_jug_loop(X, Y, Z, NewX, NewY)
    ;   CurY =:= Y
    ->  empty_y(CurY, NewY),
        format('Empty Y: (~w, ~w)~n', [CurX, NewY]),
        water_jug_loop(X, Y, Z, CurX, NewY)
    ).

% Example query:
% ?- water_jug(3, 5, 4).