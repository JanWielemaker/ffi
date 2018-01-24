:- use_module('../prolog/ffi').

/** <module> Test closure handling

This test demonstrates a simple closure.  Type extraction and validation
of closures is supported, but closure   definition through libffi is not
yet supported.
*/

:- c_import("#include \"test/test_funcptr.c\"",
            [ 'test/test_funcptr' ],
            [ test(+function(+int, [-int]), +int, [-int])
            ]).
