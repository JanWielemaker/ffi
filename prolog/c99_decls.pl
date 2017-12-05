:- module(c99_decls,
          [ c99_header_ast/2,                   % +Header, -AST
            c99_types/3                         % +Header, +Functions, -AST
          ]).
:- use_module(library(process)).
:- use_module(library(pure_input)).
:- use_module(library(apply)).
:- use_module(c99_phrase).

%!  c99_types(+Header, +Functions, -AST)
%
%

c99_types(Header, Functions, Types) :-
    c99_header_ast(Header, AST),
    maplist(prototype(AST), Functions, Types).

prototype(AST, Func, FuncDecl) :-
    FuncDecl = decl(_,
                    [ declarator(_, dd(Func, _DDS))
                    ],
                    _Attributes),
    memberchk(FuncDecl, AST).


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
