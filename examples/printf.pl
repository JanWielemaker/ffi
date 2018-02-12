/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  Public domain
*/

:- use_module(library(ffi)).

% Access variadic argument functions. This is in general not yet
% supported, but specific calling sequences are supported.  The
% example below uses the printf() function to print two floating
% point numbers.  Note that the arguments are defined as `double`
% because of the C argument promotion rules.

:- c_import("#include <stdio.h>",
            [ '-lc' ],
            [ printf(string, double, double, [void]) as printf_dd,
              printf(string, string, long,   [void]) as printf_sl
            ]).

test(X, Y) :-
    printf_dd("You entered %f and %f\n", X, Y).

print_integer_flags :-
    (   current_prolog_flag(Name, Val),
        integer(Val),
        printf_sl("%-25s %18ld\n", Name, Val),
        fail
    ;   true
    ).
