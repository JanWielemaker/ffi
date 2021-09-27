/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018, VU University Amsterdam
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

:- module(c_locations,
          [ c_lib_path/3,               % +Name, -Path, +Options
            cpp/2,                      % -Command, -Args

            ldconfig/4,                 % ?Name, ?Path, ?Version, ?Flags
            ldconfig_flush/0
          ]).
:- use_module(library(error)).
:- use_module(library(readutil)).
:- use_module(library(process)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(dcg/basics)).
:- use_module(library(filesex)).

/** <module> Define resource locations for the ffi library

This module provides the mapping from   library  names to concrete files
that can be loaded. This  is  used   by  c_import/3.  While  C compilers
typically allow one to specify a library   as, e.g., =|-lm|=, the actual
naming and physical location  of  the   file  providing  this library is
compiler and system dependent.

This module defines c_lib_path/2 to find  the concrete file implementing
a C library.  Hooks may be used to extend this predicate:

  - library_path_hook/2 is called first by c_lib_path/2 and may redefine
    the entire process.
  - cpu_alias/2 may be used if =|ldconfig -p|= is used to verify that a
    library is compatible with the current architecture of the
    SWI-Prolog process.
*/

:- multifile
    user:file_search_path/2,
    ffi:library_path_hook/3,                    % +Spec, -Path, +Options
    ffi:cpp_hook/2,				% -Command, -Argv
    ffi:compatible_architecture/1,
    ffi:cpu_alias/2.

%!  cpp(-Command, -Argv) is det.
%
%   Provide the Command and Argv for process_create/3 to call the C
%   proprocessor reading the C input from standard input.

cpp(Command, Argv) :-
    ffi:cpp_hook(Command, Argv),
    !.
cpp(path(gcc), ['-E', '-xc', -]).


%!  c_lib_path(+Spec, -Path, +Options) is det.
%
%   Find a shared object from Spec.   Spec is one of:
%
%     $ Concrete file :
%     If spec is an atom or string denoting an existing file, this
%     is used.
%     $ Alias(File) :
%     Handled to absolute_file_name/3 using the options access(execute)
%     and extensions(['',Ext]), where `Ext` is the value for the
%     Prolog flag `shared_object_extension`
%     $ Plain atom :
%     Platform dependent search.  Currently implemented for
%       - Systems that support `ldconfig -p` (e.g., Linux)
%
%   Additional search strategies may be realised   by defining rules for
%   library_path_hook/2 with the same signature.
%
%   @tbd Extend the platform specific search strategies.

%!  ffi:library_path_hook(+Name, -Path, +Options) is semidet.
%
%   Multifile hook that can  be  defined  to   resolve  a  library  to a
%   concrete file.  The hook is tried as first option by c_lib_path/2.

c_lib_path(Name, Path, Options) :-
    ffi:library_path_hook(Name, Path, Options),
    !.
c_lib_path(Name, Path, Options) :-
    atomic(Name),
    relative_file(Name, AbsName, Options),
    add_extension(AbsName, File),
    exists_file(File),
    !,
    Path = File.
c_lib_path(Spec, Path, _Options) :-
    compound(Spec),
    !,
    find_on_path(Spec, Path).
c_lib_path(Name, Path, _Options) :-
    ldconfig(Name, Path),
    !.
c_lib_path(Spec, Path, _Options) :-
    find_on_path(c_lib(Spec), Path),
    !.
c_lib_path(libc, '/usr/lib/libSystem.B.dylib', _Options) :-
    current_prolog_flag(apple, true),           % MacOS 11 hack
    !.
c_lib_path(libm, '/usr/lib/libSystem.B.dylib', _Options) :-
    current_prolog_flag(apple, true),           % MacOS 11 hack
    !.
c_lib_path(Name, _Path, _Options) :-
    existence_error(c_library, Name).

relative_file(Name, AbsName, _Options) :-
    is_absolute_file_name(Name),
    !,
    AbsName = Name.
relative_file(Name, AbsName, Options) :-
    option(relative_to(Dir), Options),
    !,
    add_arch(Dir, Dir1),
    directory_file_path(Dir1, Name, AbsName).
relative_file(Name, Name, _).

add_extension(Name, Name).
add_extension(Base, Name) :-
    current_prolog_flag(shared_object_extension, Ext),
    file_name_extension(Base, Ext, Name).

add_arch(Dir0, Dir) :-
    current_prolog_flag(arch, Arch),
    directory_file_path(Dir0, Arch, Dir).
add_arch(Dir, Dir).

find_on_path(Spec, Path) :-
    current_prolog_flag(shared_object_extension, Ext),
    absolute_file_name(Spec, Path,
                       [ access(execute),
                         extensions(['',Ext]),
                         file_errors(fail)
                       ]).


user:file_search_path(c_lib, '/usr/lib').


		 /*******************************
		 *            LDCONFIG		*
		 *******************************/

%!  ldconfig(+Name, -Path) is semidet.
%
%   Find the best matching library from the given base name

ldconfig(Name, Path) :-
    findall(l(Path, Version, Flags),
            ldconfig(Name, Path, Version, Flags),
            Candidates),
    (   Candidates = [One]
    ->  One = l(Path,_,_)
    ;   include(compatible, Candidates, Compatible),
        Compatible \== []
    ->  latest_version(Compatible, l(Path,_,_))
    ;   latest_version(Candidates, l(Path,_,_))
    ).

compatible(l(_,_,Flags)) :-
    ffi:compatible_architecture(Flags),
    !.
compatible(l(_,_,Flags)) :-
    compatible_architecture(Flags).

latest_version([One], One) :-
    !.
latest_version([H1,H2|T], Latest) :-
    version_list(H1, V1),
    version_list(H2, V2),
    (   later_version(V1, V2)
    ->  H = H1
    ;   H = H2
    ),
    latest_version([H|T], Latest).

version_list(l(_,-1,_), [-1]) :-
    !.
version_list(l(_,V,_), L) :-
    split_string(V, ".", "", L0),
    maplist(try_number, L0, L).

try_number(V, N) :-
    atom_number(V, N),
    !.
try_number(V, V).

later_version([H|T1], [H|T2]) :-
    !,
    later_version(T1, T2).
later_version(V1, V2) :-
    V1 @> V2.

%!  ldconfig(?Name, ?Path, ?Version, ?Flags) is nondet.
%
%   True when Name is the base name of a library in the ldconfig cache.
%
%   @arg Name is the base name of the library, without version or
%   extension.
%   @arg Path is the absolute file name of the library
%   @arg Version is the version extension as an atom
%   @arg Flags is a list of atoms with flags about the library

:- dynamic
    ldconfig_cache/4,
    ldconfig_cache_loaded/1.

ldconfig(Name, Path, Version, Flags) :-
    ldconfig_cache_loaded(_),
    !,
    ldconfig_cache(Name, Path, Version, Flags).
ldconfig(Name, Path, Version, Flags) :-
    with_mutex(c_locations,
               import_ldconfig_cache),
    ldconfig_cache(Name, Path, Version, Flags).

%!  ldconfig_flush
%
%   Flush the library cache maintained for ldconfig.

ldconfig_flush :-
    retractall(ldconfig_cache(_,_,_,_)),
    retractall(ldconfig_cache_loaded(_)).

import_ldconfig_cache :-
    ldconfig_cache_loaded(_), !.
import_ldconfig_cache :-
    ld_config_path(LdConfig),
    setup_call_cleanup(
        process_create(LdConfig, ['-p'],
                       [ stdout(pipe(Out))
                       ]),
        read_ldconfig(Out, 1),
        close(Out)),
    get_time(Now),
    asserta(ldconfig_cache_loaded(Now)).

ld_config_path(Path) :-
    exists_file('/sbin/ldconfig'),
    !,
    Path = '/sbin/ldconfig'.
ld_config_path(Path) :-
    absolute_file_name(path(ldconfig), Path,
                       [ access(execute),
                         file_errors(fail)
                       ]).

read_ldconfig(Out, Line) :-
    read_line_to_codes(Out, Codes),
    (   Codes == end_of_file
    ->  true
    ;   (   phrase(ldconfig_line(Name, Path, Version, Flags), Codes)
        ->  assertz(ldconfig_cache(Name, Path, Version, Flags))
        ;   Line == 1                   % first line may be comment
        ->  true
        ;   phrase(comment, Codes, _)   % Comment may also be elsewhere
        ->  true
        ;   print_message(warning, ldconfig(could_not_parse(Line, Codes)))
        ),
        Line2 is Line+1,
        read_ldconfig(Out, Line2)
    ).

ldconfig_line(Name, Path, Version, Flags) -->
    whites,
    string(NameChars), ".",
    extension(_Ext),
    opt_version(Version), !,
    whites, "(", flags(Flags), ")", whites, "=>", whites, !,
    string(PathChars), eos, !,
    { atom_codes(Name, NameChars),
      atom_codes(Path, PathChars)
    }.

extension(so)    --> "so".
extension(dylib) --> "dylib".
extension(dll)   --> "dll".

opt_version(Version) -->
    ".", version(Versions), !,
    { atomic_list_concat(Versions, '.', Version) }.
opt_version(-1) --> white.

version([H|T]) -->
    string(Chars),
    version_sep(Cont), !,
    { string_codes(H, Chars) },
    (   {Cont==true}
    ->  version(T)
    ;   {T=[]}
    ).

version_sep(true) --> ".", !.
version_sep(false) --> white.

flags([H|T]) -->
    string(Chars),
    flag_sep(Cont), !,
    { atom_codes(H, Chars) },
    (   {Cont == true}
    ->  flags(T)
    ;   {T=[]}
    ).

flag_sep(true)  --> ",", !.
flag_sep(false), ")" --> ")", !.

comment -->
    "Cache generated by:".


%!  compatible_architecture(+Flags)
%
%   Check the the library is  compatible   with  the  current SWI-Prolog
%   architecture.

compatible_architecture(Flags) :-
    cpu(CPU),
    memberchk(CPU, Flags),
    !.

cpu(CPU) :-
    current_prolog_flag(arch, Arch),
    split_string(Arch, "-", "", [CPUString|_]),
    atom_string(CPU0, CPUString),
    (   CPU = CPU0
    ;   ffi:cpu_alias(CPU0, CPU)
    ;   cpu_alias(CPU0, CPU)
    ).

%!  cpu_alias(+ArchCPU, -LDConfigCPU) is nondet.
%
%   Provide a mapping from the CPU as   known  in the Prolog `arch` flag
%   and the identifiers used by ldconfig.

cpu_alias(x86_64, 'x86-64').


		 /*******************************
		 *            MESSAGES		*
		 *******************************/

:- multifile
    prolog:message//1.

prolog:message(ldconfig(could_not_parse(Line, Codes))) -->
    [ 'ldconfig import: ~d: could not parse ~s'-[Line, Codes] ].
