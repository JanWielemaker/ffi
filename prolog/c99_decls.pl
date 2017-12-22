:- module(c99_decls,
          [ c99_header_ast/2,                   % +Header, -AST
            c99_types/3                         % +Header, +Functions, -AST
          ]).
:- use_module(library(process)).
:- use_module(library(pure_input)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(dcg/basics)).
:- use_module(c99_phrase).

%!  c99_types(+Header, +Functions, -Types)
%
%   True when Types contains the   necessary declarations for Functions.
%   Types are expanded to scalar types, structs, unions and enums.

c99_types(Header, Functions, Types) :-
    c99_header_ast(Header, AST),
    phrase(prototypes(Functions, AST), Types0),
    list_to_set(Types0, Types1),
    phrase(expand_types(Types1, Types1), Types).

prototypes([], _) --> [].
prototypes([H|T], AST) --> prototype(H, AST), prototypes(T, AST).

prototype(Func, AST) -->
    { skeleton(prototype(Return, RDecl, Params0), Func, FuncDecl),
      memberchk(FuncDecl, AST),
      maplist(param, Params0, Params),
      memberchk(type(BasicType), Return),
      pointers(RDecl, BasicType, RType)
    },
    [ function(Func, RType, Params) ],
    type_opt(RType, AST),
    types(Params, AST).

%!  skeleton(+Type, +Id, -Skeleton)
%
%   AST skeleton to find the definition of Id of type Type

skeleton(prototype(Return, RDecl, Params), Func,
         decl(Return,
              [ declarator(RDecl, dd(Func, dds(Params)))
              ],
              _Attributes)).

param(param(Specifiers, declarator(Decl, dd(Name,_))), Name-Type) :-
    memberchk(type(BasicType), Specifiers),
    pointers(Decl, BasicType, Type).

pointers(-, Type, Type).
pointers([], Type, Type).
pointers([ptr(_)|T], Basic, Type) :-
    pointers(T, *(Basic), Type).


		 /*******************************
		 *      TYPE DEFINITIONS	*
		 *******************************/

types([], _) --> [].
types([H|T], AST) --> type_opt(H, AST), types(T, AST).

type_opt(Type, AST) -->
    type(Type, AST), !.
type_opt(_, _) --> [].

type(_Name-Type, AST) --> !, type(Type, AST).
type(*(Type), AST) --> !, type(Type, AST).
type(Type, AST) -->
    { ast_type(Type, AST, Defined) },
    [ Defined ],
    type(Defined, AST).
type(type(Type), AST) -->
    type(Type, AST).
type(type(_, struct, Fields), AST) -->
    types(Fields, AST).
type(f(Types, _Declarator, _Attrs), AST) -->
    types(Types, AST).
type(type(_, typedef, Types), AST) -->
    types(Types, AST).

ast_type(struct(Name), AST, type(Name, struct, Fields)) :-
    member(decl(Specifier, _Decl, _Attrs), AST),
    memberchk(type(struct(Name, Fields)), Specifier), !.
ast_type(struct(Name, Fields), _, type(Name, struct, Fields)).
ast_type(user_type(Name), AST, type(Name, typedef, Primitive)) :-
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

expand_type(function(Name, Return, Params), _Types) --> !,
    [ function(Name, Return, Params) ].
expand_type(type(Name, struct, Fields0), Types) --> !,
    [ struct(Name, Fields) ],
    { phrase(expand_field(Fields0, Types), Fields) }.
expand_type(_, _) --> [].

expand_field([], _) --> [].
expand_field([f(Type0, Declarators, _)|T], Types) -->
    { maplist(declarator_name, Declarators, Names),
      simplify_types(Type0, Types, Type)
    },
    repeat_fields(Names, Type),
    expand_field(T, Types).

declarator_name(d(declarator(_,dd(Name,_))), Name).

repeat_fields([], _) --> [].
repeat_fields([H|T], Type) --> [f(H,Type)], repeat_fields(T, Type).

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
simplify_type(ulong)     --> [type(unsigned),type(long),type(int)].
simplify_type(uint)      --> [type(unsigned),type(int)].
simplify_type(ushort)    --> [type(unsigned),type(short)].
simplify_type(ushort)    --> [type(unsigned),type(short),type(int)].
simplify_type(uchar)     --> [type(unsigned),type(char)].
simplify_type(Type)      --> [type(Type)].
simplify_type(longlong)  --> [type(long),type(long),type(int)].
simplify_type(long)      --> [type(long),type(int)].
simplify_type(short)     --> [type(short),type(int)].


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
