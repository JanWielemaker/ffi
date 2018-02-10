:- module(test_struct,
          [ test_struct/0,
            trip_points/1
          ]).

:- use_module(library(apply_macros), []).
:- use_module(library(plunit)).

:- use_module('../prolog/ffi').

test_struct :-
    run_tests([c_struct]).

:- c_import("#include \"test_struct.c\"",
            [ 'test/test_struct' ],
            [ get_point(-struct(point), [int]),
              set_point(+struct(point), +int, +int),
              add_point(+int, +int),
              get_points([*(struct(points))]),
              clear_points()
            ]).

:- begin_tests(c_struct).

test(get_point, pt(X,Y) == pt(42,4242)) :-
    get_point(Pt, Rc),
    assertion(Rc == 0),
    c_load(Pt[x], X),
    c_load(Pt[y], Y).
test(set_point, pt(X,Y) == pt(4,7)) :-
    c_alloc(Ptr, struct(point)),
    set_point(Ptr, 4, 7),
    c_load(Ptr[x], X),
    c_load(Ptr[y], Y).
test(trip) :-
    trip_points(10).

:- end_tests(c_struct).

trip_points(Len) :-
    clear_points,
    length(Points, Len),
    maplist(random_point, Points),
    reverse(Points, RevPoints),
    add_points(RevPoints),
    point_list(L),
    assertion(Points == L).

random_point(point(X,Y)) :-
    random_between(0, 1 000 000, X),
    random_between(0, 1 000 000, Y).

add_points([]).
add_points([point(X,Y)|T]) :-
    add_point(X, Y),
    add_points(T).

point_list(L) :-
    get_points(Pts),
    point_list(Pts, L).

point_list(Pts, L) :-
    c_is_nil(Pts),
    !,
    L = [].
point_list(Pts, [point(X,Y)|T]) :-
    c_load(Pts[pt][x], X),
    c_load(Pts[pt][y], Y),
    c_load(Pts[next], Next),
    point_list(Next, T).
