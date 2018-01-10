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
          [ c_lib_path/2,               % +Name, -Path

            ldconfig/4,                 % ?Name, ?Path, ?Version, ?Flags
            ldconfig_flush/0
          ]).
:- use_module(library(error)).
:- use_module(library(readutil)).
:- use_module(library(process)).
:- use_module(library(dcg/basics)).

/** <module> Define resource locations for cinvoke

This module defines c_lib_path/2 to find  the concrete file implementing
a C library.
*/

:- multifile
    user:file_search_path/2,
    library_path_hook/2,
    compatible_architecture/1,
    cpu_alias/2.

%!  c_lib_path(+Spec, -Path) is det.
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

c_lib_path(Name, Path) :-
    library_path_hook(Name, Path),
    !.
c_lib_path(Name, Path) :-
    atomic(Name),
    exists_file(Name),
    !,
    Path = Name.
c_lib_path(Spec, Path) :-
    compound(Spec),
    !,
    current_prolog_flag(shared_object_extension, Ext),
    absolute_file_name(Spec, Path,
                       [ access(execute),
                         extensions(['',Ext])
                       ]).
c_lib_path(Name, Path) :-
    ldconfig(Name, Path, _Version, Flags),
    compatible_architecture(Flags),
    !.
c_lib_path(Name, _Path) :-
    existence_error(c_library, Name).


		 /*******************************
		 *            LDCONFIG		*
		 *******************************/

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
                       [ file_type(executable),
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
