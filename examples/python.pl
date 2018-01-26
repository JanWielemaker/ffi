:- module(python,
          [ py_call/2,                          % +Module:Call, -Return

            py_init/0,
            py_module/2,                        % +Name, -Handle
            py_function/3                       % +Module, +FuncName, -Function
          ]).
:- use_module(library(ffi)).
:- use_module(library(error)).

/** <module> Embed Python

@see https://docs.python.org/3/c-api/index.html
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

              'PyUnicode_FromString'(+string(utf8), [*('PyObject', 'MyPy_DECREF')]),
              'PyLong_FromLong'(int, [*('PyObject', 'MyPy_DECREF')]),

              'PyLong_AsLong'(*'PyObject', [-int]),

              'PyImport_Import'(*'PyObject', [*('PyObject', 'MyPy_DECREF')]),

              'PyObject_GetAttrString'(*'PyObject', +string, [*('PyObject', 'MyPy_DECREF')]),

              'PyTuple_New'(int, [*('PyObject', 'MyPy_DECREF')]),
              'PyTuple_SetItem'(*'PyObject', int, *'PyObject', [int]),
              'PyObject_CallObject'(*'PyObject', *'PyObject', [*('PyObject', 'MyPy_DECREF')]),

              'MyPy_DECREF'(*'PyObject') as 'Py_DECREF',
              'MyPy_INCREF'(*'PyObject') as 'Py_INCREF'
            ]).

:- dynamic
    py_init_done/0,
    py_module_done/2,
    py_function_done/3.

%!  py_init is det.
%
%   Initialise Python. Normally this is  called lazily. Applications may
%   wish to set `PYTHONPATH` before calling a Python interface function.

py_init :-
    py_init_done,
    !.
py_init :-
    with_mutex(python, py_init_sync).

py_init_sync :-
    py_init_done.
py_init_sync :-
    current_prolog_flag(os_argv, [Program|_]),
    'Py_SetProgramName'(Program),
    'Py_Initialize'(),
    asserta(py_init_done).

%!  py_module(+Name, -Module) is det.
%
%   Module is a Python object holding the executable module Name.

py_module(Name, Module) :-
    py_module_done(Name, Module0),
    !,
    Module = Module0.
py_module(Name, Module) :-
    with_mutex(python, py_module_sync(Name, Module0)),
    Module = Module0.

py_module_sync(Name, Module) :-
    py_module_done(Name, Module),
    !.
py_module_sync(Name, Module) :-
    py_init,
    'PyUnicode_FromString'(Name, PyString),
    'PyImport_Import'(PyString, Module),
    asserta(py_module_done(Name, Module)).

%!  py_function(+Module, +Name, -Function) is det.
%
%   Get a handle to a Python function in a module.

py_function(Module, Name, Function) :-
    py_function_done(Module, Name, Function0),
    !,
    Function = Function0.
py_function(Module, Name, Function) :-
    with_mutex(python, py_function_sync(Module, Name, Function0)),
    Function = Function0.

py_function_sync(Module, Name, Function) :-
    py_function_done(Module, Name, Function),
    !.
py_function_sync(Module, Name, Function) :-
    py_module(Module, Handle),
    'PyObject_GetAttrString'(Handle, Name, Function),
    asserta(py_function_done(Module, Name, Function)).

%!  py_call(+Call, -Return) is det.

py_call(Module:Call, Return) :-
    compound_name_arity(Call, Func, Argc),
    py_function(Module, Func, Function),
    'PyTuple_New'(Argc, Argv),
    fill_tuple(0, Argc, Call, Argv),
    'PyObject_CallObject'(Function, Argv, PyReturn),
    python_to_prolog(PyReturn, Return).

fill_tuple(I, Argc, Term, Tuple) :-
    I < Argc, !,
    I2 is I+1,
    arg(I2, Term, A),
    prolog_to_python(A, Py),
    'Py_INCREF'(Py),                            % 'PyTuple_SetItem' decrements
    'PyTuple_SetItem'(Tuple, I, Py, _Rc),
    fill_tuple(I2, Argc, Term, Tuple).
fill_tuple(_, _, _, _).

%!  prolog_to_python(+Prolog, -Python) is det.
%
%   Translate a Prolog term into a Python object.

prolog_to_python(Int, Py) :-
    integer(Int),
    !,
    'PyLong_FromLong'(Int, Py).
prolog_to_python(Prolog, _Pyton) :-
    type_error(python, Prolog).

%!  python_to_prolog(+Python, -Prolog) is det.

python_to_prolog(Py, Value) :-
    'PyLong_AsLong'(Py, Value).

