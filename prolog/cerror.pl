/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
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

:- module(c_error,
          [ posix_status/1,                     % +Status
            posix_status/4,                     % +Status, +Op, +Type, +Arg
            posix_ptr_status/1,                 % +Status
            posix_ptr_status/4,                 % +Status, +Op, +Type, +Arg
            posix_raise_error/0,
            posix_raise_error/3			% +Op, +Type, +Arg
          ]).
:- use_module(ffi).

/** <module> C interface error handling

This module provides common routines to  map error codes into apropriate
actions in Prolog. Below  is  a   typical  example  mapping the statfs()
function:

  ```
  :- module(libc_files,
            [ statfs/2
            ]).
  :- use_module(library(cinvoke)).

  :- c_import("#include <sys/vfs.h>",
              [ libc ],
              [ statfs(+string, -struct(statfs), [-int])
              ]).

  statfs(File, FsStat) :-
      statfs(File, FsStat, Status),
      posix_status(Status, statfs, file, File).
  ```
*/

cpp_const('ENOENT').
cpp_const('EPERM').
cpp_const('EACCES').
cpp_const('ENOMEM').

:- c_import("#include <string.h>
             #include <errno.h>",
            [ libc ],
            [ strerror(+int, [string]) ]).

%!  posix_status(+Code) is det.
%!  posix_status(+Code, +Action, +Type, +Argument) is det.
%
%   These predicates may be used to map  POSIX `int` error return status
%   into a suitable Prolog response. If Code is non-negative the
%   predicate simply succeeds. For other cases it retrieves the error
%   code using c_errno/1 and translates the error into a suitable Prolog
%   exception.

posix_status(Status) :-
    (   Status >= 0
    ->  true
    ;   posix_raise_error
    ).

posix_status(Status, Op, Type, Arg) :-
    (   Status >= 0
    ->  true
    ;   posix_raise_error(Op, Type, Arg)
    ).

%!  posix_ptr_status(+Code) is det.
%!  posix_ptr_status(+Code, +Action, +Type, +Argument) is det.
%
%   Handle the return code  from  POSIX   functions  that  return a NULL
%   pointer on error.

posix_ptr_status(Ptr) :-
    c_is_nil(Ptr),
    !,
    posix_raise_error.
posix_ptr_status(_).

posix_ptr_status(Ptr, Op, Type, Arg) :-
    c_is_nil(Ptr),
    !,
    posix_raise_error(Op, Type, Arg).
posix_ptr_status(_, _, _, _).


%!  posix_raise_error is det.
%!  posix_raise_error(+Action, +Type, +Argument) is det.
%
%   Raise an error from a POSIX `errno` code.
%
%   @error posix_error(Errno, String)

posix_raise_error :-
    c_errno(Errno),
    strerror(Errno, String),
    throw(error(posix_error(Errno, String), _)).

posix_raise_error(Op, Type, Arg) :-
    c_errno(Errno),
    strerror(Errno, String),
    posix_exception_context(Op, Type, Arg, String, Context),
    (   posix_exception(Errno, Op, Type, Arg, Context)
    ->  true
    ;   throw(error(posix_error(Errno, String), Context))
    ).

posix_exception_context(Op, Type, Arg, String, Context) :-
    Context = context(_Stack, posix(Op, Type, Arg, String)).

posix_exception('ENOENT', _Op, Type, Arg, Context) :- !,
    throw(error(existence_error(Type, Arg), Context)).
posix_exception('EACCES', Op, Type, Arg, Context) :- !,
    throw(error(permission_error(Op, Type, Arg), Context)).
posix_exception('EPERM', Op, Type, Arg, Context) :- !,
    throw(error(permission_error(Op, Type, Arg), Context)).
posix_exception('ENOMEM', _Op, _Type, _Arg, Context) :- !,
    throw(error(resource_error(memory), Context)).


		 /*******************************
		 *            MESSAGES		*
		 *******************************/

:- multifile
    prolog:message_context//1,
    prolog:error_message//1.

prolog:message_context(context(_, posix(Op, Type, Arg, _String))) -->
    { nonvar(Op) },
    [ ' in ~w on ~p ~p'-
      [Op, Type, Arg] ].
prolog:error_message(posix_error(Errno, String)) -->
    [ '~p (errno=~p)'-[String, Errno] ].
