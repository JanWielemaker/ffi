:- use_module(library(apply_macros)).
:- use_module(library(statistics)).

:- use_module('../prolog/cinvoke').
:- use_module('../prolog/cerror').
:- use_module('../prolog/c99_tokens').
:- use_module('../prolog/c99_phrase').
:- use_module('../prolog/c99_decls').

:- c_import("#include \"test/test_enum.c\"",
            [ 'test/test_enum' ],
            [ set_dow(+pointer, +enum(dow))
            ]).



