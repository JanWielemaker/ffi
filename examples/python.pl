% :- module(python, []).
:- use_module(library(ffi)).

/** <module> Embed Python

@see https://docs.python.org/3/extending/embedding.html
*/

:- c_import("//#define Py_LIMITED_API 1
	     #include \"examples/mypython.c\"",
            [ '-lpython3.6m',
              'examples/mypython',
              '-I/usr/include/python3.6m',
              '-I/usr/include/x86_64-linux-gnu/python3.6m'
            ],
            [ 'Py_SetProgramName'(+string(wchar_t)),
              'Py_Initialize'(),
              'PyRun_SimpleStringFlags'(+string, +'PyCompilerFlags', [-int]),
              'Py_FinalizeEx'([-int]),

              'PyUnicode_FromString'(+string(utf8), [*'PyObject']),
              'PyLong_FromLong'(int, [*'PyObject']),

              'PyLong_AsLong'(*'PyObject', [-int]),

              'PyImport_Import'(*'PyObject', [*'PyObject']),

              'PyObject_GetAttrString'(*'PyObject', +string, [*'PyObject']),

              'PyTuple_New'(int, [*'PyObject']),
              'PyTuple_SetItem'(*'PyObject', int, *'PyObject', [int]),
              'PyObject_CallObject'(*'PyObject', *'PyObject', [*'PyObject']),

              'MyPy_DECREF'(*'PyObject') as 'Py_DECREF',
              'MyPy_INCREF'(*'PyObject') as 'Py_INCREF'
            ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Issues:

  - Handle deallocation.  Py_DECREF() is a macro.  Now resolved using
    a simple wrapper function.  We need:
    - A way to avoid the c_cast/3 on a return type
    - A way to set a free function on the returned pointer
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

multiply(X,Y,Z) :-
    source_file(multiply(_,_,_), File),
    file_directory_name(File, Dir),
    setenv('PYTHONPATH', Dir),
    py_init,
    py_import("multiply", Module),
    py_call(Module, multiply(X,Y), Z).

py_init :-
    current_prolog_flag(os_argv, [Program|_]),
    'Py_SetProgramName'(Program),
    'Py_Initialize'().

py_import(File, Module) :-
    'PyUnicode_FromString'(File, PyString),
    'PyImport_Import'(PyString, Module).

py_call(Module, Call, Return) :-
    compound_name_arguments(Call, FuncName, PlArgs),
    'PyObject_GetAttrString'(Module, FuncName, Function),
    length(PlArgs, Argc),
    'PyTuple_New'(Argc, Argv),
    fill_args(PlArgs, 0, Argv),
    'PyObject_CallObject'(Function, Argv, PyReturn),
    python_to_prolog(PyReturn, Return),
    'Py_DECREF'(PyReturn).

fill_args([], _, _).
fill_args([H|T], I, Argv) :-
    prolog_to_python(H, Py),
    'PyTuple_SetItem'(Argv, I, Py, _Rc),
    I2 is I+1,
    fill_args(T, I2, Argv).

prolog_to_python(Int, Py) :-
    integer(Int),
    !,
    'PyLong_FromLong'(Int, Py).
prolog_to_python(Prolog, _Pyton) :-
    type_error(python, Prolog).

python_to_prolog(Py, Value) :-
    'PyLong_AsLong'(Py, Value).

today :-
    py_init,
    'PyRun_SimpleStringFlags'("from time import time,ctime\n\c
                               print('Today is', ctime(time()))\n",
                              null, Rc),
    writeln(Rc).
