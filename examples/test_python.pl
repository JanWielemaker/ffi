:- use_module(python).

:- initialization
    setenv('PYTHONPATH', 'examples').

multiply(X,Y,Z) :-
    py_call(multiply:multiply(X,Y), Z).
