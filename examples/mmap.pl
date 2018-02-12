/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  Public domain
*/

:- module(mmap,
          [ map_file/4,                 % +File, +Mode, -Ptr, +Options
            munmap/1,                   % +Ptr
            create_file/2,              % +File, +Size
            create_file/3               % +File, +Size, +Mode
          ]).
:- use_module(library(ffi)).
:- use_module(library(option)).
:- use_module(library(cerror)).

/** <module> Access a file using memory mapping

This module provides a simple example to access the POSIX memory mapping
capabilities. The accompagnying file test_mmap.pl exploits this file.
*/

:- meta_predicate
    map_file(+,+,-,:).

%!  cpp_const(?Const)
%
%   List the constants that we want to  know about. The argument must be
%   the a C expression that evaluates to a constant, usually the name of
%   a macro. Upon success, a fact   cpp_const(Const,  Value) is added by
%   c_import/3.

cpp_const('O_RDONLY').
cpp_const('O_WRONLY').
cpp_const('O_RDWR').
cpp_const('O_CREAT').

cpp_const('PROT_READ').
cpp_const('PROT_WRITE').

cpp_const('MAP_SHARED').
cpp_const('MAP_PRIVATE').

% Import the functions we need. Besides mmap() and munmap() we must also
% open files. Note that open has the synopsis below, i.e., is a function
% with variadic arguments.
%
%     int open(const char* name, int flags, ...);
%
% Variadic argument functions  are  only   in  part  supported: required
% arguments are processed normally and for   the remaining arguments the
% user must supply the actual C arguments   (as oppossed to the abstract
% Prolog type). Thus, for a  normal  argument   `int`  may  refer to a C
% integer of any size while for a  variadic argument it really revers to
% the C `int` type and `long`  should  be   used  if  the  argument is a
% `long`.

:- c_import("#include <sys/types.h>
             #include <sys/stat.h>
             #include <fcntl.h>
             #include <unistd.h>
             #include <sys/mman.h>",
            [ '-lc' ],
            [ open(string, int, [int]) as c_open_,
              open(string, int, int, [int]) as c_open_,
              close(int, [int]) as c_close,
              mmap(*void, int, int, int, int, int, [*void]) as mmap_,
              munmap(*void, int, [int]),
              ftruncate(int, int, [int])
            ]).

%!  map_file(+File, +Mode, -Ptr, :Options)
%
%   Simple interface to mapping a file. Ptr points at the mapped region.
%   Mode is one of `read`, `write` or `update`.  Options include
%
%     - shared(+Boolean)
%     If `true` (default `false`), modify the file on write and multiple
%     processes mapping this file get the same physical memory pages.
%     - type(:Type)
%     Cast the return into a pointer representing an array of the
%     required type. If the type is derived type (typedef, struct,
%     etc.) it is resolved in the module of the caller.

map_file(File, Mode, Ptr, M:Options) :-
    map_flags(Mode, OpenFlags, Prot),
    mmap_flags(Flags, Options),
    cast_options(Cast, M, Options),
    size_file(File, Bytes),
    setup_call_cleanup(
        c_open(File, OpenFlags, Fd),
        mmap(null, Bytes, Prot, Flags, Fd, 0, Ptr0),
        c_close(Fd)),
    cast(Cast, Bytes, Ptr0, Ptr).

map_flags(read,   'O_RDONLY', 'PROT_READ').
map_flags(write,  'O_WRONLY', 'PROT_WRITE').
map_flags(update, 'O_RDWR',   'C'('PROT_READ'|'PROT_WRITE')).

mmap_flags(Flags, Options) :-
    option(shared(true), Options),
    !,
    Flags = 'MAP_SHARED'.
mmap_flags('MAP_PRIVATE', _).

cast_options(cast(M:Type, Size, Align), M0, Options) :-
    option(type(Type0), Options, char),
    strip_module(M0:Type0, M, Type),
    c_type_size_align(M:Type, Size, Align).

cast(cast(Type, Size, _Align), Bytes, Ptr0, Ptr) :-
    Count is ((Bytes+Size-1)//Size),
    c_cast(Type[Count], Ptr0, Ptr).

%!  c_open(+File, +Flags, -Fd) is det.
%!  c_close(+Fd) is det.
%!  mmap(+Addr, +Length, +Prot, +Flags, +Fd, +Offset, -Ptr) is det.
%
%   Simple wrappers to deal with error handling.

c_open(File, Flags, Fd) :-
    c_open_(File, Flags, Fd),
    posix_status(Fd, open, file, File).

c_open(File, Flags, Mode, Fd) :-
    c_open_(File, Flags, Mode, Fd),
    posix_status(Fd, open, file, File).

c_close(Fd) :-
    c_close(Fd, Rc),
    posix_status(Rc, close, file_descriptor, Fd).

mmap(Addr, Length, Prot, Flags, Fd, Offset, Ptr) :-
    mmap_(Addr, Length, Prot, Flags, Fd, Offset, Ptr),
    posix_ptr_status(Ptr, mmap, file_descriptor, Fd).

ftruncate(Fd, Size) :-
    ftruncate(Fd, Size, Rc),
    posix_status(Rc, truncate, file_descriptor, Fd).


%!  munmap(+Addr) is det.
%
%   Unmap a mapped region.

munmap(Addr) :-
    c_dim(Addr, Count, Size),
    Length is Count*Size,
    munmap(Addr, Length).

munmap(Addr, Length) :-
    munmap(Addr, Length, Rc),
    posix_status(Rc, munmap, region, Addr-Length).

%!  create_file(+File, +Size) is det.
%!  create_file(+File, +Size, +Mode) is det.
%
%   Create a (sparse) file of Size bytes.  If File exists it is resized.
%   Mode can be specified to define the   access mode if the file didn't
%   exist before, e.g., 0o600 ensures the file is only accessible by the
%   current user.  Default is 0o666.

create_file(File, Size) :-
    create_file(File, Size, 0o666).
create_file(File, Size, Mode) :-
    setup_call_cleanup(
        c_open(File, 'C'('O_WRONLY'|'O_CREAT'), Mode, Fd),
        ftruncate(Fd, Size),
        c_close(Fd)).
