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

:- module(c99_grammar,
          [ c99_parse//1                        % -AST
          ]).
:- use_module(c99_tokens).

c99_parse(AST) -->
    c99_tokens(Tokens),
    { phrase(declarations(AST), Tokens) }.

declarations([H|T]) --> declaration(H), !, declarations(T).
declarations([]) --> [].

% A2.2. Declarations

declaration(decl(DS, I)) -->
    declaration_specifiers(DS),
    init_declarator_list(I),
    [;].

declaration_specifiers([H|T]) -->
    declaration_specifier(H), !,
    declaration_specifiers(T).
declaration_specifiers([]) --> [].

declaration_specifier(DS) --> storage_class_specifier(DS).
declaration_specifier(DS) --> type_specifier(DS).
declaration_specifier(DS) --> type_qualifier(DS).
declaration_specifier(DS) --> function_specifier(DS).

init_declarator_list([H|T]) -->
    init_declarator(H),
    !,
    (   [',']
    ->  init_declarator_list(T)
    ;   { T = [] }
    ).
init_declarator_list([]) --> [].

init_declarator(ID) -->
    declarator(D),
    (   [=]
    ->  initializer(I),
        {ID = (D=I)}
    ;   {ID = D}
    ).

storage_class_specifier(storage(typedef))  --> [typedef].
storage_class_specifier(storage(extern))   --> [extern].
storage_class_specifier(storage(static))   --> [static].
storage_class_specifier(storage(auto))     --> [auto].
storage_class_specifier(storage(register)) --> [register].

type_specifier(type(void))       --> [void].
type_specifier(type(char))       --> [char].
type_specifier(type(short))      --> [short].
type_specifier(type(int))        --> [int].
type_specifier(type(long))       --> [long].
type_specifier(type(float))      --> [float].
type_specifier(type(double))     --> [double].
type_specifier(type(signed))     --> [signed].
type_specifier(type(unsigned))   --> [unsigned].
type_specifier(type('_Bool'))    --> ['_Bool'].
type_specifier(type('_Complex')) --> ['_Complex'].
type_specifier(type(Type))       --> struct_or_union_specifier(Type).
type_specifier(type(Type))       --> enum_specifier(Type).
type_specifier(type(Type))       --> typedef_name(Type).

struct_or_union_specifier(struct(Id, Fields)) -->
    [ struct ], opt_id(Id),
    ['{'], struct_declaration_list(Fields), ['}'].
struct_or_union_specifier(struct(Id)) -->
    [ struct, id(Id) ].
struct_or_union_specifier(union(Id, Fields)) -->
    [ union ], opt_id(Id),
    ['{'], struct_declaration_list(Fields), ['}'].
struct_or_union_specifier(union(Id)) -->
    [ union, id(Id) ].

opt_id(Id) --> [id(Id)], !.
opt_id(-)  --> [].

struct_declaration_list([H|T]) -->
    struct_declaration(H), !,
    struct_declaration_list(T).
struct_declaration_list([]) --> [].

struct_declaration(f(QL, DL)) -->
    specifier_qualifier_list(QL),
    struct_declarator_list(DL),
    [;].

specifier_qualifier_list([H|T]) -->
    specifier_qualifier(H), !,
    specifier_qualifier_list(T).
specifier_qualifier_list([]) --> [].

specifier_qualifier(SQ) --> type_specifier(SQ).
specifier_qualifier(SQ) --> type_qualifier(SQ).

struct_declarator_list([H|T]) -->
    struct_declarator(H),
    (   [',']
    ->  struct_declarator_list(T)
    ;   {T=[]}
    ).

struct_declarator(SD) -->
    declarator(D),
    (   [:]
    ->  constant_expression(E),
        {SD = bitfield(D, E)}
    ;   {SD = d(D)}
    ).
struct_declarator(SD) -->
    [:], constant_expression(E),
    {SD = bitfield(-, E)}.

enum_specifier(enum(ID, EL)) -->
    [enum], opt_id(ID),
    ['{'], enumerator_list(EL), opt_comma, ['}'].
enumerator_list(enum(ID)) -->
    [enum, id(ID)].

opt_comma --> [','], !.
opt_comma --> [].

type_qualifier(const)    --> [const].
type_qualifier(restrict) --> [restrict].
type_qualifier(volatile) --> [volatile].

function_specifier(inline) --> [inline].

declarator(declarator(P, DD)) --> pointer(P), !, direct_declarator(DD).
declarator(declarator(DD))    --> direct_declarator(DD).

direct_declarator(dd(Id, DDS)) -->
    [id(Id)], !, direct_declarator_suffix(DDS).
direct_declarator(dd(D, DDS))  -->
    ['('], declarator(D), [')'], direct_declarator_suffix(DDS).

direct_declarator_suffix(dds(TQL, Ass)) -->
    ['['],
    type_qualifier_list_opt(TQL), assignment_expression_opt(Ass),
    [']'], !.
direct_declarator_suffix(dds(TQL, Ass)) -->
    ['[', static],
    type_qualifier_list_opt(TQL), assignment_expression(Ass),
    [']'], !.
direct_declarator_suffix(dds(TQL, Ass)) -->
    ['['],
    type_qualifier_list(TQL), [static], assignment_expression(Ass),
    [']'], !.
direct_declarator_suffix(dds(TQL, *)) -->
    ['['], type_qualifier_list_opt(TQL), [*,']'], !.
direct_declarator_suffix(dds(PTL)) -->
    ['('], parameter_type_list(PTL), [')'], !.
direct_declarator_suffix(dds(IDList)) -->
    ['('], identifier_list_opt(IDList), [')'], !.

pointer([ptr(TQL)|T]) -->
    [*], type_qualifier_list(TQL),
    pointers(T).

pointers([ptr(TQL)|T]) -->
    [*], type_qualifier_list(TQL), !,
    pointers(T).
pointers([]) --> [].

type_qualifier_list([H|T]) -->
    type_qualifier(H), !,
    type_qualifier_list_opt(T).

type_qualifier_list_opt([H|T]) -->
    type_qualifier(H), !,
    type_qualifier_list_opt(T).
type_qualifier_list_opt([]) --> [].

parameter_type_list([H|T]) -->
    parameter_list(H),
    (   [',']
    ->  parameter_type_list(T)
    ;   {T=[]}
    ).

parameter_list([H|T]) -->
    parameter_declaration(H),
    (   [',']
    ->  parameter_list(T)
    ;   {T=[]}
    ).

parameter_declaration(param(S,D)) -->
    declaration_specifiers(S),
    (   declarator(D)
    ->  []
    ;   abstract_declarator_opt(D)
    ).

identifier_list([H|T]) -->
    [id(H)],
    (   [',']
    ->  identifier_list(T)
    ;   {T=[]}
    ).

identifier_list_opt(IDL) -->
    identifier_list(IDL), !.
identifier_list_opt([]) --> [].

type_name(type_name(QL, D)) -->
    specifier_qualifier_list(QL), abstract_declarator_opt(D).

abstract_declarator(ad(AD,DAD)) -->
    pointer(AD), !,
    (   direct_abstract_declarator(DAD)
    ->  []
    ;   {DAD = (-)}
    ).
abstract_declarator(ad(-,DAD)) -->
    direct_abstract_declarator(DAD).

abstract_declarator_opt(AD) -->
    abstract_declarator(AD), !.
abstract_declarator_opt(ad(-,-)) --> [].

direct_abstract_declarator(dad(AD,S)) -->
    (   ['('], abstract_declarator(AD), [')']
    ->  []
    ;   {AD = (-)}
    ),
    direct_abstract_declarator_suffix(S).

direct_abstract_declarator_suffix(dads(TQL, Ass)) -->
    ['['],
    type_qualifier_list_opt(TQL), assignment_expression_opt(Ass),
    [']'], !.
direct_abstract_declarator_suffix(dads(TQL, Ass)) -->
    ['[', static],
    type_qualifier_list_opt(TQL), assignment_expression(Ass),
    [']'], !.
direct_abstract_declarator_suffix(dads(TQL, Ass)) -->
    ['['],
    type_qualifier_list(TQL), [static], assignment_expression(Ass),
    [']'], !.
direct_abstract_declarator_suffix(dads(*)) -->
    ['[',*,']'], !.
direct_abstract_declarator_suffix(dads(PTL)) -->
    ['('], parameter_type_list_opt(PTL), [')'], !.
direct_abstract_declarator_suffix(-) -->
    [].

typedef_name(typedef(Name)) -->
    [id(Name)].

initializer(init(E)) -->
    assignment_expression(E).
initializer(init(IL)) -->
    ['{'], initializer_list(IL), opt_comma, ['}'], !.

designation(=(D)) -->
    designator_list(D), [=], !.

designator_list([H|T]) -->
    designator(H),
    designator_list_opt(T).

designator_list_opt([H|T]) -->
    designator(H), !,
    designator_list_opt(T).
designator_list_opt([]) --> [].

designator([E]) -->
    constant_expression(E).
designator(.(Id)) -->
    [id(Id)].
