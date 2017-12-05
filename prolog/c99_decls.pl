:- module(c99_decls,
          [ c99_types/2                         % +Header, -AST
          ]).
:- use_module(library(process)).
:- use_module(library(pure_input)).
:- use_module(c99_phrase).

%!  c99_types(+Header, +Symbols)
%
%

c99_types(Header, AST) :-
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
