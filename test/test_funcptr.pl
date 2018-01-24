:- use_module('../prolog/ffi').

:- c_import("#include \"test/test_funcptr.c\"",
            [ 'test/test_funcptr' ],
            [ test(+function(+int, [-int]), +int, [-int])
            ]).
