:- use_module(library(apply_macros)).
:- use_module('../prolog/cinvoke').

:- dc_bind(sin(_,_), '(d)d', 'libm.so.6', 'sin').

t(N) :-
    forall(between(1, N, _),
           sin(N, _)).
t2(N) :-
    forall(between(1, N, _),
           _ is sin(N)).
