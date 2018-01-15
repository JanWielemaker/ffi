:- use_module(library(apply_macros), []).
:- use_module(library(statistics)).

:- use_module('../prolog/cinvoke').
:- use_module('../prolog/ctokens').
:- use_module('../prolog/cparser').
:- use_module('../prolog/cdecls').

:- c_import("#include \"test/test_struct.c\"",
            [ 'test/test_struct' ],
            [ get_point(-struct(point), [int]),
              set_point(+struct(point), +int, +int),
              add_point(+int, +int),
              get_points([*(struct(points))]),
              clear_points()
            ]).

ptn(N) :-
    time(forall(between(1, N, _), get_point(_,_))).

testpt :-
    c99_types("#include \"test/test.c\"",
              [ get_points,
                add_point,
                clear_points
              ], AST),
    pp(AST).

add_points([]).
add_points([point(X,Y)|T]) :-
    add_point(X, Y),
    add_points(T).

t(Len) :-
    clear_points,
    length(Points, Len),
    maplist(random_point, Points),
    reverse(Points, RevPoints),
    add_points(RevPoints),
    p(L),
    assertion(Points == L).

random_point(point(X,Y)) :-
    random_between(0, 1 000 000, X),
    random_between(0, 1 000 000, Y).

p1 :-
    get_points(Pts),
    c_load(Pts[0][pt][x], X),
    writeln(X).

p(L) :-
    get_points(Pts),
    p(Pts, L).

p(Pts, L) :-
    c_nil(Pts),
    !,
    L = [].
p(Pts, [point(X,Y)|T]) :-
    c_load(Pts[pt][x], X),
    c_load(Pts[pt][y], Y),
    c_load(Pts[next], Next),
    p(Next, T).
