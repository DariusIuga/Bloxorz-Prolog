:- dynamic detailed_mode_disabled/0.
:- dynamic debug_moves/0.
:- ensure_loaded('files.pl').


% empty_state/1
% empty_state(-SNew)
% Construiește o stare goală (fără nicio informație), care va fi dată
% primului apel set/4

empty_state(state([], (0, 0), [])).

% coordonata (0, 0) este coltul din stanga/sus (chiar dacă nu există un
% tile acolo)

% set_tile/3
% set_tile(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află o pătrățică normală.

set_tile(state(Tiles, Block, Bridges), Pos, state([tile(Pos) | Tiles], Block, Bridges)).

% set_blank/3
% set_blank(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S.
% Va fi apelat de tester doar pentru pozițiile fără pătrățele de la
% coordonate unde pentru același x și y mai mare, sau pentru același y
% și x mai mare, există pătrățele. Puteți să nu faceți nimic în acest
% predicat - depinde de cum vă reprezentați intern starea.

set_blank(State, _, State).

% set_target/3
% set_target(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află gaura (scopul).

set_target(state(Tiles, Block, Bridges), Pos, state([target(Pos) | Tiles], Block, Bridges)).

% set_fragile/3
% set_fragile(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se o pătrățică fragilă, pe care
% blocul nu poate sta în picioare.

set_fragile(state(Tiles, Block, Bridges), Pos, state([fragile(Pos) | Tiles], Block, Bridges)).

% set_block_initial/3
% set_block_initial(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află inițial blocul, plasat în
% picioare.

set_block_initial(state(Tiles, _, Bridges), Pos, state(Tiles, Pos, Bridges)).


% get_b_pos/2
% get_b_pos(+S, -BlockPos)
% Obtine pozitia sau pozitiile blocului (în funcție de dacă blocul este
% în picioare sau culcat, ca (X, Y) sau ca [(X1, Y1), (X2, Y2)]

% Bloc vertical 
get_b_pos(state(_, Pos, _), Pos) :- !.  
% Bloc orizontal
get_b_pos(state(_, (Pos1, Pos2), _), [Pos1, Pos2]).

% get_bounds/5
% get_bounds(+S, -Xmin, -Xmax, -Ymin, -Ymax).
% Obtine coordonatele limită de pe hartă la care exită celule.

get_bounds(state(Tiles, _, _), Xmin, Xmax, Ymin, Ymax) :-
    findall(X, (member(T, Tiles), T =.. [_, (X, _)]), Xs),
    findall(Y, (member(T, Tiles), T =.. [_, (_, Y)]), Ys),
    min_list(Xs, Xmin), max_list(Xs, Xmax),
    min_list(Ys, Ymin), max_list(Ys, Ymax).

% get_cell/3
% get_cell(S, Pos, Type).
% Leagă Type la tipul pătrățelei de la poziția Pos. Type trebuie legat
% la:
% tile - pentru o pătrățică obișnuită.
% fragile - pentru o pătrățică fragilă.
% target - pentru scop (gaura).
% oswitch - pentru switch de tip o.
% xswitch - pentru switch de tip x.
%
% Dacă la poziția respectivă nu se află nimic, sau este în afara
% limitelor date de get_bounds, predicatul întoarce false.

get_cell(state(Tiles, _, _), Pos, Type) :-
    ( memberchk(tile(Pos), Tiles) -> Type = tile
    ; memberchk(fragile(Pos), Tiles) -> Type = fragile
    ; memberchk(target(Pos), Tiles) -> Type = target
    ; Type = blank).

% move/3
% move(S, Move, SNext)
% Calculează în SNext starea care rezultă din realizarea mutării Move în
% starea S.
% Mutarea este una dintre d, u, l, r.
% Întoarce false dacă mutarea duce la căderea blocului în vid (nu dacă
% blocul a ajuns la scop).

move(_, _, _) :- false.


% is_final/1
% is_final(S)
% Întoarce adevărat dacă în starea S blocul este în picioare, pe aceeași
% poziție cu gaura (scopul).

is_final(state(Tiles, Pos, _)) :-
    memberchk(target(Pos), Tiles).
    
% pentru etapa 2
% set_switch(+S, +Pos, +Switch, +Func, +Positions, SOut)
% Switch: o sau x
% Func: switch, uponly sau dnonly
% Position: pozitiile podului
set_switch(S, P, _, _, _, SNew) :- set_tile(S, P, SNew).
