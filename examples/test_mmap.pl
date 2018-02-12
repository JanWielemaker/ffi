/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  Public domain
*/

:- module(test_mmap,
          [ random_point_file/2         % +File, +Count
          ]).
:- use_module(library(ffi)).
:- use_module(mmap).

/** <module> Demo file mapping and type definition

This file demonstrates how the mmap.pl demo can be used. The task we set
outselves is to create a file  holding   N  instances  of `struct point'
consisting of two floats and fill the file with random floats.

In native Prolog this is not very  easy   as  there  is no proper way to
access the binary representation of floats   or  deal with the structure
layout.
*/

% Define the structure type.

:- c_struct(point,
            [ f(x, double),
              f(y, double)
            ]).

%!  random_point_file(+File, +Count)
%
%   Create File, holding Count instances of `struct point` with a random
%   `x` and `y`.
%
%   Take these steps:
%
%     1. Compute the required size
%     2. Create the file
%     3. Map it in memory as an array of the structures
%     4. Fill the array
%     5. unmap the file

random_point_file(File, Count) :-
    c_type_size_align(struct(point), Size, _),
    Bytes is Size*Count,
    create_file(File, Bytes),
    setup_call_cleanup(
        map_file(File, update, Ptr,
                 [ type(struct(point)),
                   shared(true)
                 ]),
        fill_points(0, Size, Ptr),
        munmap(Ptr)).

fill_points(I, Size, Ptr) :-
    I < Size,
    !,
    X is random_float,
    Y is random_float,
    c_store(Ptr[I][x], X),
    c_store(Ptr[I][y], Y),
    I2 is I+1,
    fill_points(I2, Size, Ptr).
fill_points(_, _, _).
