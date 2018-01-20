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

:- module(c99_decls,
          [ c99_header_ast/2,           % +Header, -AST
            c99_types/3,                % +Header, +Functions, -AST
            c99_types/4,                % +Header, +Functions, -AST, -Consts
            ast_constant/2		% +AST, -Constant
          ]).
:- use_module(library(process)).
:- use_module(library(pure_input)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(dcg/basics)).
:- use_module(cparser).
:- use_module(ffi, [c_sizeof/2]).

%!  c99_types(+Header, +Functions, -Types) is det.
%!  c99_types(+Header, +Functions, -Types, Consts) is det.
%
%   True when Types contains the   necessary declarations for Functions.
%   Types are expanded to scalar types, structs, unions and enums.

c99_types(Header, Functions, Types) :-
    c99_types(Header, Functions, Types, -).
c99_types(Header, Functions, Types, Consts) :-
    c99_header_ast(Header, AST),
    phrase(prototypes(Functions, AST), Types0),
    list_to_set(Types0, Types1),
    phrase(expand_types(Types1, Types1), Types),
    constants(AST, Consts).

prototypes([], _) --> [].
prototypes([H|T], AST) --> prototype(H, AST), prototypes(T, AST).

prototype(Func, AST) -->
    { skeleton(prototype(Return, RDecl, Params0), Func, FuncDecl),
      memberchk(FuncDecl, AST), !,
      parameters(Params0, Params),
      memberchk(type(BasicType), Return),
      pointers(RDecl, BasicType, RType)
    },
    [ function(Func, RType, Params) ],
    type_opt(RType, AST, [], Resolved),
    types(Params, AST, Resolved, _).
prototype(Func, _) -->
    { print_message(error, ffi(existence_error(function, Func))),
      fail
    }.

%!  skeleton(+Type, +Id, -Skeleton)
%
%   AST skeleton to find the definition of Id of type Type

skeleton(prototype(Return, RDecl, Params), Func,
         decl(Return,
              [ declarator(RDecl, dd(Func, dds(Params)))
              ],
              _Attributes)).
skeleton(prototype(Return, RDecl, Params), Func,
         function(Return,
                  declarator(RDecl, dd(Func, dds(Params))),
                  _Attributes,
                  _Block)).


parameters([param([type(void)], _)], []) :- %fail,
    !.
parameters(Params0, Params) :-
    maplist(param, Params0, Params).

param(param(Specifiers, declarator(Decl, dd(Name,_))), Name-Type) :-
    !,
    memberchk(type(BasicType), Specifiers),
    pointers(Decl, BasicType, Type).
param(param(Specifiers, ad(Decl, dad(-, -))), Type) :-
    memberchk(type(BasicType), Specifiers),
    pointers(Decl, BasicType, Type).

pointers(-, Type, Type).
pointers([], Type, Type).
pointers([ptr(_)|T], Basic, Type) :-
    pointers(T, *(Basic), Type).


		 /*******************************
		 *      TYPE DEFINITIONS	*
		 *******************************/

%!  types(+Types, +AST, +Resolved0, -Resolved)// is det.
%
%   Create a simplified representation of   Types using the declarations
%   in AST.

types([], _, Resolved, Resolved) --> [].
types([H|T], AST, Resolved0, Resolved) -->
    type_opt(H, AST, Resolved0, Resolved1),
    types(T, AST, Resolved1, Resolved).

type_opt(Type, AST, Resolved0, Resolved) -->
    type(Type, AST, Resolved0, Resolved), !.
type_opt(_, _, Resolved, Resolved) --> [].

type(Type, _AST, Resolved, Resolved) -->
    { memberchk(Type, Resolved) },
    !.
type(_Name-Type, AST, R0, R) --> !, type(Type, AST, R0, R).
type(*(Type), AST, R0, R) --> !, type(Type, AST, R0, R).
type(Type, AST, R0, R) -->
    { ast_type(Type, AST, Defined) },
    [ Defined ],
    type(Defined, AST, [Type|R0], R).
type(type(Type), AST, R0, R) -->
    type(Type, AST, R0, R).
type(type(_, struct, Fields), AST, R0, R) -->
    types(Fields, AST, R0, R).
type(type(_, union, Fields), AST, R0, R) -->
    types(Fields, AST, R0, R).
type(type(_, enum, _Members), _AST, R, R) -->
    [].
type(f(Types, _Declarator, _Attrs), AST, R0, R) -->
    types(Types, AST, R0, R).
type(type(_, typedef, Types), AST, R0, R) -->
    types(Types, AST, R0, R).

ast_type(struct(Name), AST, type(Name, struct, Fields)) :-
    member(decl(Specifier, _Decl, _Attrs), AST),
    memberchk(type(struct(Name, Fields)), Specifier), !.
ast_type(union(Name), AST, type(Name, union, Fields)) :-
    member(decl(Specifier, _Decl, _Attrs), AST),
    memberchk(type(union(Name, Fields)), Specifier), !.
ast_type(union(Name, Fields), _, type(Name, union, Fields)).
ast_type(struct(Name, Fields), _, type(Name, struct, Fields)).
ast_type(type(enum(Name, Members)), _, type(Name, enum, Members)).
ast_type(user_type(Name), AST, type(Name, typedef, Primitive)) :-
    typedef(Name, AST, Primitive).

typedef(Name, AST, Primitive) :-
    member(decl(Specifier,
                [ declarator(_, dd(Name, _))], _Attrs), AST),
    selectchk(storage(typedef), Specifier, Primitive), !.


		 /*******************************
		 *          EXPAND TYPES	*
		 *******************************/

expand_types([], _) --> [].
expand_types([H|T], Types) -->
    expand_type(H, Types),
    expand_types(T, Types).

expand_type(function(Name, Return0, Params0), Types) --> !,
    { untypedef(Types, Return0, Return),
      maplist(untypedef(Types), Params0, Params)
    },
    [ function(Name, Return, Params) ].
expand_type(type(Name, struct, Fields0), Types) --> !,
    [ struct(Name, Fields) ],
    { phrase(expand_field(Fields0, Types), Fields) }.
expand_type(type(Name, union, Fields0), Types) --> !,
    [ union(Name, Fields) ],
    { phrase(expand_field(Fields0, Types), Fields) }.
expand_type(type(Name, enum, Members), _Types) --> !,
    [ enum(Name, Members) ].
expand_type(type(Name, typedef, Type0), Types) --> !,
    { simplify_types(Type0, Types, Type1),
      type_reference(Type1, Type)
    },
    [ typedef(Name, Type) ].
expand_type(_, _) --> [].

expand_field([], _) --> [].
expand_field([f(Type0, Declarators, _)|T], Types) -->
    { maplist(declarator_name, Declarators, Names),
      simplify_types(Type0, Types, Type)
    },
    repeat_fields(Names, Type),
    expand_field(T, Types).

declarator_name(d(declarator(Ptr,dd(Name,dds([],AST)))),
                array(Name, N, Ptr)) :-
    ast \== (-),
    ast_constant(AST, N),
    !.
declarator_name(d(declarator(Ptr,dd(Name,_))),
                plain(Name, Ptr)).

repeat_fields([], _) --> [].
repeat_fields([H|T], Type) --> field(H, Type), repeat_fields(T, Type).

field(plain(Name, Ptr), Type0) -->
    { pointers(Ptr, Type0, Type1),
      type_reference(Type1, Type)
    },
    [f(Name, Type)].
field(array(Name, Length, Ptr), EType0) -->
    { type_reference(EType0, EType),
      pointers(Ptr, array(EType,Length), Type)
    },
    [f(Name, Type)].

simplify_types(Type0, Types, Type) :-
    phrase(expand_user_type(Type0, Types), Type1),
    (   phrase(simplify_type(Type, Types), Type1)
    ->  true
    ;   print_message(error, ctypes(cannot_simplify(Type0))),
        Type = Type0
    ).

expand_user_type([], _) --> [].
expand_user_type([type(user_type(TypeName))|T], Types) --> !,
    { memberchk(type(TypeName, typedef, Expanded), Types) },
    string(Expanded),
    expand_user_type(T, Types).
expand_user_type([H|T], Types) -->
    [H],
    expand_user_type(T, Types).

simplify_type(struct(Name,Fields), Types) -->
    [ type(struct(Name, Fields0)) ],
    !,
    { phrase(expand_field(Fields0, Types), Fields) }.
simplify_type(Type, _Types) -->
    simplify_type(Type).

simplify_type(ulonglong) --> [type(unsigned),type(long),type(long),type(int)].
simplify_type(ulonglong) --> [type(long),type(long),type(unsigned),type(int)].
simplify_type(ulong)     --> [type(unsigned),type(long)].
simplify_type(ulong)     --> [type(unsigned),type(long),type(int)].
simplify_type(ulong)     --> [type(long),type(unsigned),type(int)].
simplify_type(uint)      --> [type(unsigned),type(int)].
simplify_type(ushort)    --> [type(unsigned),type(short)].
simplify_type(ushort)    --> [type(unsigned),type(short),type(int)].
simplify_type(ushort)    --> [type(short),type(unsigned),type(int)].
simplify_type(uchar)     --> [type(unsigned),type(char)].
simplify_type(Type)      --> [type(Type)].
simplify_type(longlong)  --> [type(long),type(long),type(int)].
simplify_type(long)      --> [type(long),type(int)].
simplify_type(short)     --> [type(short),type(int)].

untypedef(Types, *(Type0), *(Type)) :-
    !,
    untypedef(Types, Type0, Type).
untypedef(Types, Name-Type0, Name-Type) :-
    !,
    untypedef(Types, Type0, Type).
untypedef(Types, user_type(Name), Type) :-
    simplify_types([type(user_type(Name))], Types, Type1),
    type_reference(Type1, Type),
    !.
untypedef(_, Type, Type).

type_reference(struct(Name, _Fields), struct(Name)) :- !.
type_reference(union(Name, _Fields),  union(Name)) :- !.
type_reference(enum(Name, _Values),   enum(Name)) :- !.
type_reference(Type,                  Type).



		 /*******************************
		 *              EVAL		*
		 *******************************/

%!  ast_constant(+AST, -Constant) is det.
%
%   Evaluate an AST expression to a constant.
%
%   @tbd: complete operators. Clarify what  to   do  with  limited range
%   integers and overflows.

ast_constant(i(V), V).
ast_constant(l(Int), Int).
ast_constant(ll(Int), Int).
ast_constant(u(Int), Int).
ast_constant(ul(Int), Int).
ast_constant(ull(Int), Int).
ast_constant(float(Float), Float).
ast_constant(double(Float), Float).
ast_constant(char(Codes), Codes).
ast_constant(wchar(Codes), Codes).
ast_constant(sizeof(Type), Size) :-
    (   ast_sizeof(Type, Size)
    ->  true
    ;   print_message(warning, c(failed, sizeof(Type))),
        fail
    ).
ast_constant(o(Op, L), C) :-
    ast_constant(L, LC),
    c_op(Op, LC, C).
ast_constant(o(Op, L, R), C) :-
    ast_constant(L, LC),
    ast_constant(R, RC),
    c_op(Op, LC, RC, C).

c_op(+, A, A).
c_op(-, A, V) :- V is -A.
c_op(~, A, V) :- V is \A.
c_op(!, A, V) :- ebool(A, B), neg(B, V).

c_op(*,    L, R, V) :- V is L*R.
c_op(/,    L, R, V) :- V is L/R.
c_op('%',  L, R, V) :- V is L mod R.
c_op(+,    L, R, V) :- V is L + R.
c_op(-,    L, R, V) :- V is L - R.
c_op(<<,   L, R, V) :- V is L << R.
c_op(>>,   L, R, V) :- V is L >> R.
c_op(<,    L, R, V) :- (L < R -> V = 1 ; V = 0).
c_op(>,    L, R, V) :- (L > R -> V = 1 ; V = 0).
c_op(>=,   L, R, V) :- (L >= R -> V = 1 ; V = 0).
c_op(<=,   L, R, V) :- (L =< R -> V = 1 ; V = 0).
c_op(==,   L, R, V) :- (L =:= R -> V = 1 ; V = 0).
c_op('!=', L, R, V) :- (L =\= R -> V = 1 ; V = 0).
c_op(&,    L, R, V) :- V is L /\ R.
c_op('|',  L, R, V) :- V is L \/ R.
c_op(^,    L, R, V) :- V is L xor R.
c_op(&&,   L, R, V) :- ebool(L, LB), ebool(R, RB), and(LB, RB, V).
c_op('||', L, R, V) :- ebool(L, LB), ebool(R, RB), or(LB, RB, V).

ebool(V, 0) :- V =:= 0, !.
ebool(_, 1).

neg(0, 1).
neg(1, 0).

and(1, 1, 1) :- !.
and(_, _, 0).

or(1, 1, 1) :- !.
or(0, 1, 1) :- !.
or(1, 0, 1) :- !.
or(0, 0, 0) :- !.

%!  ast_sizeof(+Type, -Size)
%
%   Determine the size of an AST type expression.
%
%   @tbd: complete with user defined types, general expressions.

ast_sizeof(type(type_name([type(Primitive)],ad(-,dad(-,-)))), Size) :-
    c_sizeof(Primitive, Size).


		 /*******************************
		 *            CONSTANTS		*
		 *******************************/

%!  constants(+AST, -Constants) is det.
%
%   Constants is a list Name=SubAST that  provides the value for defined
%   constants.

constants(_AST, Constants) :-
    Constants == (-),                           % constants are not demanded
    !.
constants(AST, Constants) :-
    findall(Name=Value, constant(AST, Name, Value), Constants).

constant(AST, Name, Value) :-
    member(decl([storage(static),type(int)],
                [declarator(-,dd(MagicName,-))=init(Value)],
                _),
           AST),
    atom_concat('__swipl_const_', Name, MagicName),
    Value \== Name.


		 /*******************************
		 *       CALL PREPROCESSOR	*
		 *******************************/

%!  c99_header_ast(+Header, -AST)

c99_header_ast(Header, AST) :-
    setup_call_cleanup(
        open_gcc_cpp(Header, In),
        phrase_from_stream(c99_parse(AST), In),
        close(In)).

open_gcc_cpp(Header, Out) :-
    process_create(path(gcc), ['-E', '-xc', -],
                   [ stdin(pipe(In)),
                     stdout(pipe(Out))
                   ]),
    thread_create(
        setup_call_cleanup(
            open_string(Header, HIn),
            copy_stream_data(HIn, In),
            (   close(HIn),
                close(In)
            )), _, [detached(true)]).


		 /*******************************
		 *             MESSAGES		*
		 *******************************/

:- multifile prolog:message//1.

prolog:message(ffi(Msg)) -->
    message(Msg).

message(existence_error(function, Func)) -->
    [ 'FFI: cannot find function ~q'-[Func] ].
