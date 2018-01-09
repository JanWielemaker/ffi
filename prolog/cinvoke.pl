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

:- module(cinvoke,
          [ c_import/3,                 % +Header, +Libs, +Functions

                                        % Memory access predicates
            c_calloc/4,                 % -Ptr, +Type, +Size, +Count
            c_free/1,                   % +Ptr
            c_typeof/2,                 % +Ptr, -Type
            c_load/4,                   % +Ptr, +Offset, +Type, -Value
            c_store/4,                  % +Ptr, +Offset, +Type, +Value
            c_offset/6,                 % +Ptr0, +Off, +Type, +Size, +Count, -Ptr
            c_sizeof/2,                 % +Type, -Bytes
            c_alignof/2,                % +Type, -Bytes

            c_alloc/2,			% -Ptr, +Type
            c_load/2,                   % +Location, -Value
            c_store/2,                  % +Location, +Value

            c_struct/2,                 % +Name, +Fields

            c_current_enum/3,           % :Id, ?Enum, ?Value
            c_current_struct/1,         % :Name
            c_current_struct/3,         % :Name, -Size, -Alignment
            c_current_struct_field/4,   % :Name, ?Field, ?Offset, ?Type

            c_struct_dict/2,            % ?Ptr,  ?Dict

            c_enum_in/3,                % :Id, +Enum, -Int
            c_enum_out/3,               % :Id, +Enum, +Int

            c_alloc_string/3,           % -Ptr, +Data, +Encoding
            c_load_string/4,            % +Ptr, -Data, +Type, +Encoding

            c_errno/1,                  % -Integer

            op(20, yf, [])
          ]).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(apply)).
:- use_module(library(pairs)).
:- use_module(c99_decls).

/** <module> Bind Prolog predicates to C functions
*/

:- meta_predicate
    c_current_enum(:,?,?),
    c_enum_in(:,+,-),
    c_enum_out(:,+,+),
    c_current_struct(:),
    c_current_struct(:,?,?),
    c_current_struct_field(:,?,?,?),
    c_struct_dict(:,?).


:- use_foreign_library(foreign(cinvoke4pl)).

:- multifile
    user:file_search_path/2,
    system:term_expansion/2,
    user:exception/3,
    c_function/3.


user:file_search_path(dc, '/lib/x86_64-linux-gnu'). % Ubuntu
user:file_search_path(dc, '/usr/lib64').            % Fedora
user:file_search_path(dc, '.').


		 /*******************************
		 *    LIBRARIES AND SYMBOLS	*
		 *******************************/

%!  dc_load_library(+Path, -Handle)
%
%   Load the given library

%!  dc_free_library(+Handle)
%
%   Free a given library

%!  dc_find_symbol(+Handle, +Name, -FuncPtr)
%
%   True when FuncPtr is a pointer to Name in the library Handle

%!  ic_context(-Context)
%
%   Global context for the cinvoke library

:- dynamic  ci_context_cache/1.
:- volatile ci_context_cache/1.

ci_context(Ctx) :-
    ci_context_cache(Ctx0),
    !,
    Ctx = Ctx0.
ci_context(Ctx) :-
    with_mutex(cinvoke, ci_context_sync(Ctx0)),
    Ctx = Ctx0.

ci_context_sync(Ctx) :-
    ci_context_cache(Ctx),
    !.
ci_context_sync(Ctx) :-
    ci_context_create(Ctx),
    asserta(ci_context_cache(Ctx)).


%!  ci_library(+Base, -FHandle)
%
%   Find a file handle for a foreign library

:- dynamic  ci_library_cache/2.
:- volatile ci_library_cache/2.

ci_library(Base, FHandle) :-
    ci_library_cache(Base, FHandle0),
    !,
    FHandle = FHandle0.
ci_library(Base, FHandle) :-
    with_mutex(dyncall, ci_library_sync(Base, FHandle)).

ci_library_sync(Base, FHandle) :-
    ci_library_cache(Base, FHandle0),
    !,
    FHandle = FHandle0.
ci_library_sync(Base, FHandle) :-
    absolute_file_name(dc(Base), Path,
                       [ access(read),
                         file_type(executable)
                       ]),
    ci_context(Ctx),
    ci_library_create(Ctx, Path, FHandle),
    assertz(ci_library_cache(Base, FHandle)).


		 /*******************************
		 *             IMPORT		*
		 *******************************/

%!  c_import(+Header, +Libs, +Functions)
%
%   Import Functions as predicates from Libs   based  on the declaration
%   from Header.

c_import(Header, Libs, Functions) :-
        throw(error(context_error(nodirective,
                                  c_import(Header, Libs, Functions)), _)).

system:term_expansion((:- c_import(Header, Libs, Functions)),
                      Clauses) :-
    prolog_load_context(module, M),
    maplist(functor_name, Functions, FunctionNames),
    add_constants(M, Header, HeaderConst),
    c99_types(HeaderConst, FunctionNames, Types, Constants),
    phrase(( c_constants(Constants),
             c_import(Libs, Functions, FunctionNames, Types)),
           Clauses).

functor_name(Spec, Name) :-
    functor(Spec, Name, _).

c_import(Libs, Functions, FunctionNames, Types) -->
    decls(Types),
    compile_types(Types, Types),
    wrap_functions(Functions, Types),
    libs(Libs, FunctionNames).

decls(_) -->
    [ (:- discontiguous(('$c_lib'/2,
                         '$c_struct'/3,
                         '$c_struct_field'/4))) ].

compile_types([], _) --> [].
compile_types([struct(Name,Fields)|T], Types) --> !,
    compile_struct(Name, Fields, Types),
    compile_types(T, Types).
compile_types([enum(Name, Values)|T], Types) --> !,
    compile_enum(Name, Values),
    compile_types(T, Types).
compile_types([_|T], Types) --> !,
    compile_types(T, Types).

wrap_functions([], _) --> [].
wrap_functions([H|T], Types) -->
    wrap_function(H, Types), wrap_functions(T, Types).

wrap_function(Signature, Types) -->
    { Signature =.. [Name|SigArgs],
      memberchk(function(Name, Ret, Params), Types),
      length(SigArgs, Arity),
      matching_signature(Name, SigArgs, Ret, Params),
      functor(Head, Name, Arity),
      prolog_load_context(module, M)
    },
    [ cinvoke:c_function(M:Head, Params, Ret),
      (:- dynamic(Name/Arity)),
      (Head :- cinvoke:define(M:Head, SigArgs))
    ].

matching_signature(Name, SigArgs, Ret, Params) :-
    append(RealArgs, [[_]], SigArgs),
    !,
    (   same_length(RealArgs, Params)
    ->  true
    ;   print_message(error, cinvoke(nonmatching_params(SigArgs, Params))),
        fail
    ),
    (   Ret == void
    ->  print_message(error, cinvoke(void_function(Name))),
        fail
    ;   true
    ).
matching_signature(Name, SigArgs, Ret, Params) :-
    !,
    (   same_length(SigArgs, Params)
    ->  true
    ;   print_message(error, cinvoke(nonmatching_params(SigArgs, Params))),
        fail
    ),
    (   Ret == void
    ->  true
    ;   print_message(warning, cinvoke(nonvoid_function(Name, Ret)))
    ).

libs([], _) --> [].
libs([H|T], Functions) --> [ '$c_lib'(H, Functions) ], libs(T, Functions).

%!  define(:Signature, +Params, +Ret)
%
%   Actually link the C function

:- public
    define/2.

define(Signature, SigArgs) :-
    Signature = M:_Head,
    link_clause(Signature, SigArgs, Clause),
    asserta(M:Clause),
    call(Signature).

link_clause(M:Goal, SigArgs,
            (Head :- !, Body)) :-
    c_function(M:Goal, ParamSpec, RetType),
    pairs_values(ParamSpec, ParamTypes),
    phrase(signature_string(ParamTypes), ParamChars),
    atom_codes(Params, ParamChars),
    (   RetType == void
    ->  Ret = ''
    ;   phrase(signature_string([RetType]), RetChars),
        atom_codes(Ret, RetChars)
    ),
    functor(Goal, Name, Arity),
    functor(Head, Name, Arity),
    functor(Head1, Name, Arity),
    (   M:'$c_lib'(Lib, Funcs),
        memberchk(Name, Funcs),
        ci_library(Lib, FH),
        ci_library_load_entrypoint(FH, Name, FuncPtr)
    ->  debug(ctypes, 'Binding ~p (Ret=~p, Params=~p)', [Name, Ret, Params]),
        ci_function_create(FuncPtr, cdecl, Ret, Params, Prototype)
    ;   existence_error(c_function, Name)
    ),
    convert_args(SigArgs, 1, Arity, Head, Head1, PreConvert, PostConvert),
    Invoke = cinvoke:ci_function_invoke(Prototype, Head1),
    mkconj(PreConvert, Invoke, Body0),
    mkconj(Body0, PostConvert, Body).

convert_args([], _, _, _, _, true, true).
convert_args([H|T], I, Arity, Head0, Head1, GPre, GPost) :-
    arg(I, Head0, Arg0),
    arg(I, Head1, Arg1),
    (   convert_arg(H, Arg0, Arg1, GPre1, GPost1)
    ->  true
    ;   Arg0 = Arg1,
        GPre1 = true,
        GPost1 = true
    ),
    I2 is I + 1,
    convert_args(T, I2, Arity, Head0, Head1, GPre2, GPost2),
    mkconj(GPre1, GPre2, GPre),
    mkconj(GPost1, GPost2, GPost).

% parameter values
convert_arg(-struct(Name), Ptr, Ptr,
            c_alloc(Ptr, struct(Name)),
            true).
convert_arg(+string(Enc),  String, Ptr,
            c_alloc_string(Ptr, String, Enc),
            true).
convert_arg(+string, String, Ptr, Pre, Post) :-
    convert_arg(+string(text), String, Ptr, Pre, Post).
convert_arg(+enum(Enum), Id, Int,
            c_enum_in(Id, Enum, Int),
            true).
convert_arg(-enum(Enum), Id, Ptr,
            c_alloc(Ptr, enum(Enum)),
            c_load(Ptr, Id)).

% return value
convert_arg([-string(Enc)], String, Ptr,
            true,
            c_load_string(Ptr, String, string, Enc)).
convert_arg([-string], String, Ptr, Pre, Post) :-
    convert_arg([-string(text)], String, Ptr, Pre, Post).
convert_arg([-enum(Enum)], Id, Int,
            true,
            c_enum_out(Id, Enum, Int)).

mkconj(true, G, G) :- !.
mkconj(G, true, G) :- !.
mkconj(G1, G2, (G1,G2)).

%!  signature_string(+Types)//
%
%   Get string description of the argument for the C layer

signature_string([]) --> [].
signature_string([H|T]) --> signature(H), signature_string(T).

signature(char)      --> "hhi".
signature(uchar)     --> "uhhi".
signature(short)     --> "hi".
signature(ushort)    --> "uhi".
signature(int)       --> "i".
signature(uint)      --> "ui".
signature(long)      --> "li".
signature(ulong)     --> "uli".
signature(longlong)  --> "lli".
signature(ulonglong) --> "ulli".
signature(float)     --> "f".
signature(double)    --> "lf".
signature(*(_))      --> "p".
signature(enum(_))   --> "i".


		 /*******************************
		 *          STRUCTURES		*
		 *******************************/

%!  c_struct(+Name, +Fields)
%
%   Declare a C structure with name  Name.   Fields  is  a list of field
%   specifications of the form:
%
%     - f(Name, Type)
%
%   Where Type is one of
%
%     - A primitive type (`char`, `uchar`, ...)
%     - struct(Name)
%     - union(Name)
%     - enum(Name)
%     - *(Type)
%     - array(Type, Size)
%
%   A structure declaration is compiled into a number of clauses

c_struct(Name, Fields) :-
    throw(error(context_error(nodirective, c_struct(Name, Fields)), _)).

system:term_expansion((:- c_struct(Name, Fields)), Clauses) :-
    phrase(compile_structs([struct(Name, Fields)]), Clauses).

compile_structs(List) -->
    compile_structs(List, List).

compile_structs([], _) --> [].
compile_structs([struct(Name,Fields)|T], All) -->
    compile_struct(Name, Fields, All),
    compile_structs(T, All).

compile_struct(Name, Fields, All) -->
    field_clauses(Fields, Name, 0, End, 0, Alignment, All),
    { Size is Alignment*((End+Alignment-1)//Alignment) },
    [ '$c_struct'(Name, Size, Alignment) ].

field_clauses([], _, End, End, Align, Align, _) --> [].
field_clauses([f(Name,Type)|T], Struct, Off0, Off, Align0, Align, All) -->
    { type_size_align(Type, Size, Alignment, All),
      Align1 is max(Align0, Alignment),
      Off1 is Alignment*((Off0+Alignment-1)//Alignment),
      Off2 is Off1 + Size
    },
    [ '$c_struct_field'(Struct, Name, Off1, Type) ],
    field_clauses(T, Struct, Off2, Off, Align1, Align, All).


%!  type_size(+Type, -Size)
%
%   Size is the size of an object of Type.

type_size(Type, Size) :-
    type_size_align(Type, Size, _).

%!  type_size_align(+Type, -Size, -Alignment) is det.
%
%   True when Type must be aligned at Alignment and is of size Size.

type_size_align(Type, Size, Alignment) :-
    type_size_align(Type, Size, Alignment, []).

type_size_align(Type, Size, Alignment, _All) :-
    c_alignof(Type, Alignment),
    !,
    c_sizeof(Type, Size).
type_size_align(struct(Name), Size, Alignment, All) :-
    memberchk(struct(Name, Fields), All), !,
    phrase(compile_struct(Name, Fields, All), Clauses),
    memberchk('$c_struct'(Name, Size, Alignment), Clauses).
type_size_align(struct(Name, Fields), Size, Alignment, All) :-
    phrase(compile_struct(Name, Fields, All), Clauses),
    memberchk('$c_struct'(Name, Size, Alignment), Clauses).
type_size_align(struct(Name), Size, Alignment, _) :-
    '$c_struct'(Name, Size, Alignment),
    !.
type_size_align(array(Type,Len), Size, Alignment, All) :-
    !,
    type_size_align(Type, Size0, Alignment, All),
    Size is Size0*Len.
type_size_align(enum(_Enum), Size, Alignment, _) :-
    !,
    c_alignof(int, Alignment),
    c_sizeof(int, Size).
type_size_align(*(_), Size, Alignment, _) :-
    !,
    c_alignof(pointer, Alignment),
    c_sizeof(pointer, Size).
type_size_align(Type, _Size, _Alignment, _) :-
    existence_error(type, Type).

%!  c_current_struct(:Name) is nondet.
%!  c_current_struct(:Name, ?Size, ?Align) is nondet.
%
%   Total size of the struct in bytes and alignment restrictions.

c_current_struct(Name) :-
    c_current_struct(Name, _, _).
c_current_struct(M:Name, Size, Align) :-
    current_predicate(M:'$c_struct'/3),
    M:'$c_struct'(Name, Size, Align).

%!  c_current_struct_field(:Name, ?Field, ?Offset, ?Type)
%
%   Fact to provide efficient access to fields

c_current_struct_field(M:Name, Field, Offset, Type) :-
    current_predicate(M:'$c_struct_field'/4),
    M:'$c_struct_field'(Name, Field, Offset, Type).


%!  c_alloc(-Ptr, +TypeAndInit) is det.
%
%   Allocate memory for a C object of  Type and optionally initialse the
%   data. TypeAndInit can take several forms:
%
%     $ A plain type :
%     Allocate an array to hold a single object of the given type.
%
%     $ Type[Count] :
%     Allocate an array to hold _Count_ objects of _Type_.
%
%     $ Type[] = Init :
%     If Init is data that can be used to initialize an array of
%     objects of Type, allocate an array of sufficient size and
%     initialize each element with data from Init.  The following
%     combinations of Type and Init are supported:
%
%       $ char[] = Text :
%       Where Text is a valid Prolog representation for text: an
%       atom, string, list of character codes or list of characters.
%       The Prolog Unicode data is encoded using the native multibyte
%       encoding of the OS.
%
%       $ char(Encoding)[] = Text :
%       Same as above, using a specific encoding.  Encoding is one of
%       `text` (as above), `utf8` or `iso_latin_1`.
%
%       $ Scalar[] = List :
%       If the type is a basic C scalar type and the data is a list,
%       allocate an array of the length of the list and store each
%       element in the corresponding location of the array.
%
%       $ Type = Value :
%       Same as =|Type[] = [Value]|=.
%
%   @tbd: error generation
%   @tbd: support enum and struct initialization from atoms and
%   dicts.

c_alloc(Ptr, Type = Data) :-
    !,
    c_init(Type, Data, Ptr).
c_alloc(Ptr, Type[Count]) :-
    !,
    type_size(Type, Size),
    c_calloc(Ptr, Type, Size, Count).
c_alloc(Ptr, Type) :-
    type_size(Type, Size),
    c_calloc(Ptr, Type, Size, 1).

c_init(Type[], Data, Ptr) :-
    !,
    c_init_array(Type, Data, Ptr).
c_init(Type, Data, Ptr) :-
    atom(Type),
    !,
    type_size(Type, Size),
    c_calloc(Ptr, Type, Size, 1),
    c_store(Ptr, 0, Type, Data).

c_init_array(char, Data, Ptr) :-
    !,
    c_alloc_string(Ptr, Data, text).
c_init_array(char(Encoding), Data, Ptr) :-
    !,
    c_alloc_string(Ptr, Data, Encoding).
c_init_array(Type, List, Ptr) :-
    atom(Type),                                 % primitive type
    is_list(List),
    length(List, Len),
    type_size(Type, Size),
    c_calloc(Ptr, Type, Size, Len),
    fill_array(List, 0, Ptr, Size, Type).

fill_array([], _, _, _, _).
fill_array([H|T], Offset, Ptr, Size, Type) :-
    c_store(Ptr, Offset, Type, H),
    Offset2 is Offset+Size,
    fill_array(T, Offset2, Ptr, Size, Type).


%!  c_load(+Ptr, -Value) is det.
%
%   Load a C value indirect over Ptr. Ptr   may be of the form Ptr[I] to
%   access the nth element of the array or.
%
%   @tbd: enum

c_load(Spec, Value) :-
    c_address(Spec, Ptr, Offset, Type),
    c_load_(Ptr, Offset, Type, Value).

c_load_(Ptr, Offset, Type, Value) :-
    (   atom(Type)
    ->  c_load(Ptr, Offset, Type, Value)
    ;   compound_type(Type)
    ->  type_size(Type, Size),
        c_offset(Ptr, Offset, Type, Size, 1, Value)
    ;   Type = array(EType, Len)
    ->  type_size(Type, ESize),
        c_offset(Ptr, Offset, EType, ESize, Len, Value)
    ;   Type = enum(Enum)
    ->  c_load(Ptr, Offset, int, IntValue),
        c_enum_out(Value, Enum, IntValue)
    ;   domain_error(type, Type)
    ).

compound_type(struct(_)).
compound_type(union(_)).

%!  c_store(:Location, +Value)

c_store(Spec, Value) :-
    c_address(Spec, Ptr, Offset, Type),
    c_store_(Ptr, Offset, Type, Value).

c_store_(Ptr, Offset, Type, Value) :-
    (   atom(Type)
    ->  c_store(Ptr, Offset, Type, Value)
    ;   Type = enum(Set)
    ->  c_enum_in(Value, Set, IntValue),
        c_store_(Ptr, Offset, int, IntValue)
    ).


%!  c_address(+Spec, -Ptr, -Offset, -Type)
%
%   Translate a specification into a pointer, offset and type.

c_address(Spec[E], Ptr, Offset, Type) :-
    !,
    c_address(Spec, Ptr0, Offset0, Type0),
    (   atom(E)
    ->  c_member(Type0, E, Ptr0, Offset0, Ptr, Offset, Type)
    ;   integer(E)
    ->  c_array_element(Type0, E, Ptr0, Offset0, Ptr, Offset, Type)
    ;   type_error(member_selector, E)
    ).
c_address(Ptr, Ptr, 0, Type) :-
    c_typeof(Ptr, Type).

c_array_element(Type, E, Ptr, Offset0, Ptr, Offset, Type) :-
    type_size(Type, ESize),
    Offset is Offset0+E*ESize.

c_member(struct(Struct), Field, Ptr, Offset0, Ptr, Offset, EType) :-
    !,                                          % should always be this
    '$c_struct_field'(Struct, Field, FOffset, EType),
    Offset is Offset0+FOffset.
c_member(Struct, Field, Ptr, Offset0, Ptr, Offset, EType) :-
    '$c_struct_field'(Struct, Field, FOffset, EType),
    Offset is Offset0+FOffset.


		 /*******************************
		 *             DICT		*
		 *******************************/

%!  c_struct_dict(:Struct, ?Dict)
%
%   Translate between a struct and a dict

c_struct_dict(M:Ptr, Dict) :-
    nonvar(Ptr),
    !,
    c_typeof(Ptr, Type),
    (   Type = struct(Name)
    ->  findall(f(Field, Offset, FType),
                c_current_struct_field(M:Name, Field, Offset, FType),
                Fields),
        maplist(get_field(Ptr), Fields, Pairs),
        dict_pairs(Dict, Name, Pairs)
    ;   domain_error(c_struct_pointer, Ptr)
    ).

get_field(Ptr, f(Name, Offset, Type), Name-Value) :-
    c_load_(Ptr, Offset, Type, Value).


		 /*******************************
		 *            ENUM		*
		 *******************************/

%!  c_current_enum(:Id, ?Enum, ?Value)
%
%   True when Id is a member of Enum with Value.

c_current_enum(M:Id, Enum, Value) :-
    current_predicate(M:'$c_struct'/3),
    M:'$enum'(Id, Enum, Value).

%!  c_enum_in(:Id, +Enum, -Value) is det.
%
%   Convert an input element for an enum to a value.

c_enum_in(Id, Enum, Value) :-
    c_current_enum(Id, Enum, Value),
    !.
c_enum_in(Id, Enum, _Value) :-
    existence_error(enum_id, Id, Enum).

%!  c_enum_in(:Id, +Enum, +Value) is det.
%
%   Convert an input element for an enum to a value.

c_enum_out(Id, Enum, Value) :-
    c_current_enum(Id, Enum, Value),
    !.
c_enum_out(_Id, Enum, Value) :-
    existence_error(enum_value, Value, Enum).

%!  compile_enum(+Name, +Values)// is det.
%
%   Compile an enum declaration into clauses for '$enum'/3.

compile_enum(Name, Values) -->
    enum_clauses(Values, 0, Name).

enum_clauses([], _, _) --> [].
enum_clauses([enum_value(Id, -)|T], I, Name) -->
    !,
    [ '$enum'(Id, Name, I) ],
    { I2 is I + 1 },
    enum_clauses(T, I2, Name).
enum_clauses([enum_value(Id, C)|T], _, Name) -->
    { ast_constant(C, I) },
    [ '$enum'(Id, Name, I) ],
    { I2 is I + 1 },
    enum_clauses(T, I2, Name).


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


		 /*******************************
		 *        CPP CONSTANTS		*
		 *******************************/

add_constants(Module, Header0, Header) :-
    current_predicate(Module:cpp_const/1),
    findall(Const, Module:cpp_const(Const), Consts),
    Consts \== [],
    !,
    must_be(list(atom), Consts),
    maplist(const_decl, Consts, Decls),
    atomics_to_string([Header0|Decls], "\n", Header).
add_constants(_, Header, Header).

const_decl(Const, Decl) :-
    format(string(Decl), "static int __swipl_const_~w = ~w;", [Const, Const]).

c_constants([]) --> [].
c_constants([H|T]) --> c_constant(H), c_constants(T).

c_constant(Name=AST) -->
    { ast_constant(AST, Value) },
    !,
    [ cpp_const(Name, Value) ].
c_constant(Name=AST) -->
    { print_message(warning, c(not_a_constant(Name, AST))) }.


		 /*******************************
		 *           EXPANSION		*
		 *******************************/

cpp_expand(Modules, T0, T) :-
    atom(T0),
    member(M, Modules),
    call(M:cpp_const(T0, T)),
    !.
cpp_expand(Modules, T0, T) :-
    compound(T0),
    !,
    compound_name_arguments(T0, Name, Args0),
    maplist(cpp_expand(Modules), Args0, Args1),
    compound_name_arguments(T1, Name, Args1),
    (   T0 == T1
    ->  T = T0
    ;   T = T1
    ).
cpp_expand(_, T, T).

system:term_expansion(T0, T) :-
    prolog_load_context(module, M),
    current_predicate(M:cpp_const/2),
    cpp_expand([M], T0, T),
    T0 \== T.

		 /*******************************
		 *        LOW LEVEL DOCS	*
		 *******************************/

%!  c_calloc(-Ptr, +Type, +Size, +Count) is det.
%
%   Allocate a chunk of memory similar to   the C calloc() function. The
%   chunk is associated with the created Ptr,   a _blob_ of type `c_ptr`
%   (see blob/2). The content of the chunk   is  filled with 0-bytes. If
%   the blob is garbage collected  by   the  atom  garbage collector the
%   allocated chunk is freed.
%
%   @arg Type is the represented C type.  It is either an atom or a term
%   of the shape struct(Name), union(Name) or enum(Name).  The atomic
%   type name is not interpreted.  See also c_typeof/2.
%   @arg Size is the size of a single element in bytes, i.e., should be
%   set to sizeof(Type).  As this low level function doesn't know how
%   large a structure or union is, this figure must be supplied by the
%   high level predicates.
%   @arg Count is the number of elements in the array.

%!  c_free(+Ptr) is det.
%
%   Free the chunk associated with Ptr. This   may be used to reduce the
%   memory foodprint without waiting for the atom garbage collector. The
%   blob itself can only be reclaimed by the atom garbage collector.

%!  c_load(+Ptr, +Offset, +Type, -Value) is det.
%
%   Fetch a C arithmetic value of Type at Offset from the pointer. Value
%   is unified with an integer or floating  point number. If the size of
%   the chunk behind the pointer is  known,   Offset  is validated to be
%   inside the chunk represented by Ptr.  Pointers may

%!  c_offset(+Ptr0, +Offset, +Type, +Size, +Count, -Ptr) is det.
%
%   Get a pointer to some  location  inside   the  chunk  Ptr0.  This is
%   currently used to get a stand-alone pointer  to a struct embedded in
%   another struct or a struct from an  array of structs. Note that this
%   is *not* for accessing pointers inside a struct.
%
%   Creating a pointer inside an existing chunk increments the reference
%   count of Ptr0. Reclaiming the two pointers requires two atom garbage
%   collection cycles, one to reclaim  the   sub-pointer  Ptr and one to
%   reclaim Ptr0.
%
%   The c_offset/5 primitive can also be used to _cast_ a pointer, i.e.,
%   reinterpret its contents as if  the  pointer   points  at  data of a
%   different type.

%!  c_store(+Ptr, +Offset, +Type, +Value) is det.
%
%   Store a C scalar value of type Type  at Offset into Ptr. If Value is
%   a pointer, its reference count is incremented   to  ensure it is not
%   garbage collected before Ptr is garbage collected.

%!  c_typeof(+Ptr, -Type) is det.
%
%   True when Type is the Type used   to  create Ptr using c_calloc/4 or
%   c_offset/6.
%
%   @arg Type is an atom or term of the shape struct(Name), union(Name)
%   or enum(Name).

%!  c_sizeof(+Type, -Bytes) is semidet.
%
%   True when Bytes is the size of the C scalar type Type. Only supports
%   basic C types.  Fails silently on user defined types.

%!  c_alignof(+Type, -Bytes) is semidet.
%
%   True when Bytes is the mininal alignment for the C scalar type Type.
%   Only supports basic C types. Fails   silently on user defined types.
%   This value is used to compute the layout of structs.
