/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2021, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(ffi_array,
          [ fa_create/4,                % -Array, +DIM, +Type, +Default
            fa_val/3,                   % +Cell, +Array, -Value
            fa_setval/3,                % +Cell, +Array, +Value
            fa_b_setval/3,              % +Cell, +Array, +Value
            fa_fill/2                   % +Array ,+Value
          ]).
:- use_module(library(ffi)).

/** <module> Provide native global arrays

This is a simple example  to  use   the  FFI  library  memory management
routines to implement an array as a native C global array.
*/

%!  fa_create(-Array, +DIM, +Type, +Default) is det.
%
%   Create a foreign N-dimensional array for   elements of Type, filling
%   all elements with Default.

fa_create(Array, DIM, Type, Default) :-
    carth_list(DIM, Cubes, Length),
    c_type(Type, CType),
    c_alloc(Ptr, CType[Length]),
    Array = [](DIM, Cubes, Ptr),
    fill(0, Length, Ptr, Default).

%!  fa_val(+Cell, +Array, -Value) is det.
%
%   True when Value is the value for Cell in Array.

fa_val(Cell, [](DIM, Cubes, Ptr), Value) =>
    a_index_0(Cell, DIM, Cubes, Index),
    c_load(Ptr[Index], Value).

%!  fa_setval(+Cell, +Array, +Value) is det.
%
%   Set the value for Cell in Array to Value.

fa_setval(Cell, [](DIM, Cubes, Ptr), Value) =>
    a_index_0(Cell, DIM, Cubes, Index),
    c_store(Ptr[Index], Value).

%!  fa_b_setval(+Cell, +Array, +Value) is det.
%
%   Set the value for Cell in Array to  Value. The change is reverted on
%   backtracking.

fa_b_setval(Cell, [](DIM, Cubes, Ptr), Value) =>
    a_index_0(Cell, DIM, Cubes, Index),
    c_load(Ptr[Index], Old),
    (   Old =@= Value
    ->  true
    ;   c_store(Ptr[Index], Value),
        undo(c_store(Ptr[Index], Old))
    ).

%!  fa_fill(+Array, +Value)
%
%   Fill the entire array with Value

fa_fill([](_DIM, [Length|_Cubes], Ptr), Value) =>
    fill(0, Length, Ptr, Value).

c_type(Type, Type).

fill(I, Length, Ptr, Default) :-
    I < Length,
    !,
    c_store(Ptr[I], Default),
    I2 is I+1,
    fill(I2, Length, Ptr, Default).
fill(_, _, _, _).


a_index_0(Pos, Dims, [_|Cubes], Index) :-
    (   a_index(Pos, Dims, Cubes, 0, Index)
    ->  true
    ;   domain_error(array(Dims), Pos)
    ).

a_index([H], [DH], [], I0, Index) :-
    !,
    DH0 is DH-1,
    between(0, DH0, H),
    Index is I0+H.
a_index([H|T], [DH|DT], [CH|CT], I0, Index) :-
    DH0 is DH-1,
    between(0, DH0, H),
    I1 is I0+H*CH,
    a_index(T, DT, CT, I1, Index).

carth_list([One], Cube, Dim) =>
    Cube = [One],
    Dim = One.
carth_list([H|T], Cube, Dim) =>
    Cube = [Dim|Cube0],
    carth_list(T, Cube0, Dim0),
    Dim is Dim0*H.
