:- use_module('../prolog/ffi').
:- use_module('../prolog/cdecls').
:- use_module(library(pprint)).

du :-
    debug(c99(unit)).

:- c_import("#include \"test_struct.c\"",
            [ 'test_struct' ],
            [ dim_dim(+int, -int)
            ]).

t :-
    c99_types("#include \"test/test_struct.c\"", [],
              [ set_point
              ],
              Types),
    print_term(Types, []).

te :-
    c99_types("#include <string.h>
               #include <errno.h>", [],
	      [ strerror,
                strncmp
	      ],
	      Types),
    print_term(Types, []).

tw :-
    c99_types("#include <wchar.h>", [],
	      [ wcslen
	      ],
	      Types),
    print_term(Types, []).

ts :-
    c99_types("#include <sys/stat.h>", [],
	      [ stat
	      ],
	      Types),
    print_term(Types, []).

tf :-
    c99_types("#include \"test/test_funcptr.c\"", [],
              [ test
              ],
              Types),
    print_term(Types, []).


ast(Header) :-
    c99_header_ast(Header, [], AST),
    pp(AST).
