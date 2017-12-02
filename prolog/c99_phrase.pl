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
    { phrase(translation_unit(AST), Tokens) }.


		 /*******************************
		 *       A.2.1 Expression	*
		 *******************************/

primary_expression(E) --> [id(E)].
primary_expression(E) --> constant(E).
primary_expression(E) --> string_literal(E).
primary_expression(E) --> ['('], expression(E), [')'].

constant(i(I)) --> [i(I)].
constant(l(I)) --> [l(I)].
constant(ll(I)) --> [ll(I)].
constant(u(I)) --> [u(I)].
constant(ul(I)) --> [ul(I)].
constant(ull(I)) --> [ull(I)].
constant(float(F)) --> [float(F)].
constant(double(D)) --> [double(D)].
constant(enum_value(Name)) --> [enum_value(Name)].
constant(char(Codes)) --> [char(Codes)].
constant(wchar(Codes)) --> [wchar(Codes)].

string_literal(str(S)) --> [str(S)].
string_literal(wstr(S)) --> [wstr(S)].

postfix_expression(e(P, PF)) -->
    primary_expression(P),
    expression_postfix(PF).

expression_postfix(array(I)) -->
    ['['], expression(I), [']'].
expression_postfix(args(List)) -->
    ['('], argument_expression_list_opt(List), [')'].
expression_postfix(.(Id)) -->
    [ '.', id(Id) ].
expression_postfix(++) -->
    [++].
expression_postfix(--) -->
    [--].
expression_postfix(cast(Type, Init)) -->
    ['('], type_name(Type), [')', '{'],
    initializer_list(Init), opt_comma, ['}'].

argument_expression_list_opt([H|T]) -->
    assignment_expression(H),
    argument_expression_list_opt(T).
argument_expression_list_opt([]) --> [].

unary_expression(E) -->
    postfix_expression(E).
unary_expression(++(UE)) -->
    [++], unary_expression(UE).
unary_expression(--(UE)) -->
    [--], unary_expression(UE).
unary_expression(op(Op, Expr)) -->
    unary_operator(Op),
    cast_expression(Expr).
unary_expression(sizeof(Expr)) -->
    [sizeof], unary_expression(Expr).
unary_expression(sizeof(type(Type))) -->
    [sizeof, '('], type_name(Type), [')'].

unary_operator(&) --> [&].
unary_operator(*) --> [*].
unary_operator(+) --> [+].
unary_operator(-) --> [-].
unary_operator(~) --> [~].
unary_operator(!) --> [!].

cast_expression(Expr) -->
    unary_expression(Expr).
cast_expression(cast(Type, Expr)) -->
    ['('], type_name(Type), [')'], cast_expression(Expr).

multiplicative_expression(Expr) -->
    cast_expression(A),
    (   multiplicative_op(Op)
    ->  multiplicative_expression(B),
        { re_nest(Op, A, B, Expr) }
    ;   { Expr = A }
    ).

multiplicative_op(*) --> [*].
multiplicative_op(/) --> [/].
multiplicative_op('%') --> ['%'].

re_nest(Op, A, o(Op2, B, C), Expr) :-
    re_nest(Op2, o(Op,A,B), C, Expr).
re_nest(Op, A, B, o(Op, A, B)).

additive_expression(Expr) -->
    multiplicative_expression(A),
    (   additive_op(Op)
    ->  additive_expression(B),
        { re_nest(Op, A, B, Expr) }
    ;   { Expr = A }
    ).

additive_op(+) --> [+].
additive_op(-) --> [-].

shift_expression(Expr) -->
    additive_expression(A),
    (   shift_op(Op)
    ->  shift_expression(B),
        { re_nest(Op, A, B, Expr) }
    ;   { Expr = A }
    ).

shift_op(<<) --> [<<].
shift_op(>>) --> [>>].

relational_expression(Expr) -->
    shift_expression(A),
    (   relational_op(Op)
    ->  relational_expression(B),
        { re_nest(Op, A, B, Expr) }
    ;   { Expr = A }
    ).

relational_op(<) --> [<].
relational_op(>) --> [>].
relational_op(>=) --> [>=].
relational_op(<=) --> [<=].

equality_expression(Expr) -->
    relational_expression(A),
    (   equality_op(Op)
    ->  equality_expression(B),
        { re_nest(Op, A, B, Expr) }
    ;   { Expr = A }
    ).

equality_op(==) --> [==].
equality_op('!=') --> ['!='].

and_expression(Expr) -->
    equality_expression(A),
    (   [&]
    ->  and_expression(B),
        { re_nest(&, A, B, Expr) }
    ;   { Expr = A }
    ).

exclusive_or_expression(Expr) -->
    and_expression(A),
    (   [^]
    ->  exclusive_or_expression(B),
        { re_nest(^, A, B, Expr) }
    ;   { Expr = A }
    ).


inclusive_or_expression(Expr) -->
    exclusive_or_expression(A),
    (   ['|']
    ->  inclusive_or_expression(B),
        { re_nest('|', A, B, Expr) }
    ;   { Expr = A }
    ).

logical_and_expression(Expr) -->
    inclusive_or_expression(A),
    (   [&&]
    ->  logical_and_expression(B),
        { re_nest(&&, A, B, Expr) }
    ;   { Expr = A }
    ).

logical_or_expression(Expr) -->
    logical_and_expression(A),
    (   ['||']
    ->  logical_or_expression(B),
        { re_nest('||', A, B, Expr) }
    ;   { Expr = A }
    ).

conditional_expression(Expr) -->
    logical_or_expression(A),
    (   [?]
    ->  expression(If),
        [:],
        conditional_expression(Then),
        { Expr = cond(A, If, Then) }
    ;   { Expr = A }
    ).

assignment_expression(Expr) -->
    conditional_expression(Expr).
assignment_expression(assign(Op, UE, AE)) -->
    unary_expression(UE),
    assignment_operator(Op),
    assignment_expression(AE).

assignment_expression_opt(Expr) -->
    assignment_expression(Expr).
assignment_expression_opt(-) --> [].

assignment_operator(=)    --> [=].
assignment_operator(*=)   --> [*=].
assignment_operator(/=)   --> [/=].
assignment_operator('%=') --> ['%='].
assignment_operator(+=)   --> [+=].
assignment_operator(-=)   --> [-=].
assignment_operator(<<=)  --> [<<=].
assignment_operator(>>=)  --> [>>=].
assignment_operator(&=)   --> [&=].
assignment_operator(^=)   --> [^=].
assignment_operator('|=') --> ['|='].

expression(Expr) -->
    assignment_expression(A),
    (   [',']
    ->  expression(B),
        { re_nest(=, A, B, Expr) }
    ;   { Expr = A }
    ).

constant_expression(E) -->
    conditional_expression(E).


		 /*******************************
		 *      A2.2. Declarations	*
		 *******************************/

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

parameter_type_list_opt(List) -->
    parameter_type_list(List).
parameter_type_list_opt([]) --> [].

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

initializer_list([H|T]) -->
    initializer1(H), !,
    initializer_list(T).
initializer_list([]) --> [].

initializer1(init(D,I)) -->
    designation(D), !,
    initializer(I).
initializer1(init(-,I)) -->
    initializer(I).

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

		 /*******************************
		 *       A.2.3 Statements	*
		 *******************************/

statement(S) --> labeled_statement(S).
statement(S) --> compound_statement(S).
statement(S) --> expression_statement(S).
statement(S) --> selection_statement(S).
statement(S) --> iteration_statement(S).
statement(S) --> jump_statement(S).

labeled_statement(label(L, Statement)) -->
    [id(L), [:]], !, statement(Statement).
labeled_statement(case(V, Statement)) -->
    [case], constant_expression(V), [:], !, statement(Statement).
labeled_statement(default(Statement)) -->
    [default, :], !, statement(Statement).

compound_statement(block(Statements)) -->
    ['{'], block_item_list_opt(Statements), ['}'].

block_item_list_opt([H|T]) -->
    block_item(H), !,
    block_item_list_opt(T).
block_item_list_opt([]) --> [].

block_item(H) --> declaration(H).
block_item(H) --> statement(H).

expression_statement(E) -->
    expression_opt(E), [;], !.

expression_opt(E) -->
    expression(E), !.
expression_opt(void) -->
    [].

selection_statement(if(Cond, If, Then)) -->
    [if, '('], expression(Cond), [')'],
    statement(If),
    (   [else]
    ->  statement(Then)
    ;   {Then = void}
    ).
selection_statement(switch(Expr, Statement)) -->
    [switch, '('], expression(Expr), [')'],
    statement(Statement).

iteration_statement(while(Expr, Statement)) -->
    [while, '('], expression(Expr), [')'], statement(Statement).
iteration_statement(do_while(Expr, Statement)) -->
    [do], statement(Statement), [while, '('], expression(Expr), [')', ';'].
iteration_statement(for(Init, Cond, Iter, Statement)) -->
    [for, '('], expression_opt(Init), [;], expression_opt(Cond), [;],
    expression_opt(Iter), [')'], statement(Statement).
iteration_statement(for2(Decl, Expr1, Expr2, Statement)) -->
    [for, '('], declaration(Decl), expression_opt(Expr1), [;],
    expression_opt(Expr2), [')'], statement(Statement).

jump_statement(goto(Id)) -->
    [ goto, id(Id) ].
jump_statement(continue) -->
    [ continue ].
jump_statement(break) -->
    [ break ].
jump_statement(return(Expr)) -->
    [ return ], expression(Expr).

		 /*******************************
		 *            A.2.4		*
		 *******************************/

translation_unit([H|T]) -->
    external_declaration(H), !,
    translation_unit(T).
translation_unit([]) --> [].

external_declaration(D) --> function_definition(D).
external_declaration(D) --> declaration(D).

function_definition(function(Specifiers, Declarator, Params, Body)) -->
    declaration_specifiers(Specifiers),
    declarator(Declarator),
    declaration_list_opt(Params),
    compound_statement(Body).

declaration_list_opt([H|T]) -->
    declaration(H), !,
    declaration_list_opt(T).
declaration_list_opt([]) --> [].
