:- module(test_python,
          [ test_python/0
          ]).
:- use_module(python).
:- use_module(library(plunit)).

test_python :-
    run_tests([ python
              ]).

:- initialization
    source_file(test_python, File),
    file_directory_name(File, Dir),
    setenv('PYTHONPATH', Dir).

:- begin_tests(python).

test(multiply, Z == 6) :-
    py_call(demo:multiply(2,3), Z).
test(multiply, Z == 6.8) :-
    py_call(demo:multiply(2,3.4), Z).
test(concat, Z == "aapnoot") :-
    py_call(demo:concat("aap", "noot"), Z).
test(concat, Z == "aapno\u0000ot") :-
    py_call(demo:concat("aap", "no\u0000ot"), Z).
test(concat, Z == [1,2,3]) :-
    py_call(demo:concat([1], [2,3]), Z).
test(dict, Z == py{name:"bob", age:42}) :-
    py_call(demo:trivial(py{name:"bob", age:42}), Z).
test(bool, Z == true) :-
    py_call(demo:trivial(true), Z).
test(bool, Z == false) :-
    py_call(demo:trivial(false), Z).

:- end_tests(python).
