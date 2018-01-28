

:- module(python,
          [ py_call/2,                          % +Module:Call, -Return

            py_init/0,
            py_module/2,                        % +Name, -Handle
            py_function/3                       % +Module, +FuncName, -Function
          ]).
:- use_module(library(ffi)).
:- use_module(library(error)).
:- use_module(library(apply)).

/** <module> Embed Python

This demo embeds the Python interpreter in   Prolog using the Prolog FFI
bridge.  Quite  likely  is  is  worthwhile   to  implement  a  low-level
replacement  of  this  library  for  a    robust  and  high  performance
connection. This module has been implemented  to evaluate the FFI bridge
to C.

This module proves that the FFI interface is powerful and compact. While
slower than what can be achieved using a native interface, the FFI based
interface  performs  reasonably  for  many  applications.  For  example,
transferring two 100,000 (integer) lists two   Python, appending them in
Python and transferring the 200,000 long list back to Prolog takes about
0.5 second (Intel mobile i7  CPU).  It   is  easy  to port time critical
functions to C to improve the performance   while FFI based interface is
easily expanded to support a larger  part   of  the  rich Python foreign
interface.

Issues:

  - Unfortunately, some of the Python API is defined as macros.
    We need to implement these as functions.  This requires creating
    and managing a shared object.
  - The current interface doesn't support Prolog threads.  This
    is because decrementing the Python reference counts is done
    by the atom garbage collector that may run in any Prolog thread,
    but must be executed in the Python thread that owns the reference.

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
              'PyRun_SimpleStringFlags'(string, 'PyCompilerFlags', [int]),
              'Py_FinalizeEx'([int]),

              'PyLong_FromLongLong'(int, [*('PyObject', 'MyPy_DECREF')]),
              'PyFloat_FromDouble'(float, [*('PyObject', 'MyPy_DECREF')]),
              'PyUnicode_FromString'(+string(utf8),
                                     [*('PyObject', 'MyPy_DECREF')]),
              'PyUnicode_FromWideChar'(+string(wchar_t), +int,
                                       [*('PyObject', 'MyPy_DECREF')]),

              'MyPyLong_Check'(*'PyObject', [-int]) as 'PyLong_Check',
              'MyPyBool_Check'(*'PyObject', [-int]) as 'PyBool_Check',
              'MyPyFloat_Check'(*'PyObject', [-int]) as 'PyFloat_Check',
              'MyPyUnicode_Check'(*'PyObject', [-int]) as 'PyUnicode_Check',
              'MyPyList_Check'(*'PyObject', [-int]) as 'PyList_Check',
              'MyPyDict_Check'(*'PyObject', [-int]) as 'PyDict_Check',

              'PyLong_AsLongLong'(*'PyObject', [-int]),
              'PyBool_FromLong'(int, [*('PyObject', 'MyPy_DECREF')]),
              'PyFloat_AsDouble'(*'PyObject', [-float]),
              'PyUnicode_AsWideCharString'(*'PyObject', -int,
                                           [*(wchar_t, 'PyMem_Free')]),

              'PyImport_Import'(*'PyObject', [*('PyObject', 'MyPy_DECREF')]),

              'PyObject_GetAttrString'(*'PyObject', +string,
                                       [*('PyObject', 'MyPy_DECREF')]),

              'PyTuple_New'(int, [*('PyObject', 'MyPy_DECREF')]),
              'PyTuple_SetItem'(*'PyObject', int, *'PyObject', [int]),

              'PyList_New'(int, [*('PyObject', 'MyPy_DECREF')]),
              'PyList_Append'(*'PyObject', *'PyObject', [int]),
              'PyList_GetItem'(*'PyObject', int, [*('PyObject')]),
              'PyList_Size'(*'PyObject', [int]),

              'PyDict_New'([*('PyObject', 'MyPy_DECREF')]),
              'PyDict_SetItemString'(*'PyObject', string(utf8), *'PyObject', [int]),
              'PyDict_SetItem'(*'PyObject', *'PyObject', *'PyObject', [int]),
              'PyDict_Next'(*'PyObject', *int,
                            *(*('PyObject')), *(*('PyObject')), [int]),

              'PyObject_CallObject'(*'PyObject', *'PyObject',
                                    [*('PyObject', 'MyPy_DECREF')]),

              'PyErr_Occurred'([*('PyObject')]),
              'PyErr_Clear'(),

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
%   See setenv/2.
%
%   @tbd Currently disables the  Prolog  GC   thread  as  objects cannot
%   receive  a  Py_DECREF()  from  another    thread  that  created  the
%   reference.

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
    set_prolog_gc_thread(false),
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
    py_check_exception,
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
    py_check_exception,
    asserta(py_function_done(Module, Name, Function)).

%!  py_call(+Call, -Return) is det.

py_call(Module:Call, Return) :-
    compound_name_arity(Call, Func, Argc),
    py_function(Module, Func, Function),
    'PyTuple_New'(Argc, Argv),
    fill_tuple(0, Argc, Call, Argv),
    'PyObject_CallObject'(Function, Argv, PyReturn),
    py_check_exception,
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
%   Translate a Prolog term into a Python object. Supported translations
%   are in the table below. Note that   atoms are translated to strings,
%   except for the boolean atoms. This implies that arbitrary atoms that
%   must be translated to strings must first   be translated to a Prolog
%   string.  Integer support uses the C type =long long= as intermediate
%   and is thus limited to 64 bits on must machines.
%
%     | Prolog            | Python |
%     ------------------------------
%     | integer           | Long   |
%     | float             | Float  |
%     | `true` or `false` | Bool   |
%     | atom		  | String |
%     | string		  | String |
%     | list		  | List   |
%     | dict		  | Dict   |

prolog_to_python(Prolog, Py) :-
    (   integer(Prolog)
    ->  'PyLong_FromLongLong'(Prolog, Py)
    ;   float(Prolog)
    ->  'PyFloat_FromDouble'(Prolog, Py)
    ;   string(Prolog)
    ->  prolog_string_to_python(Prolog, Py)
    ;   atom(Prolog)
    ->  prolog_atom_to_python(Prolog, Py)
    ;   is_list(Prolog)
    ->  'PyList_New'(0, Py),
        maplist(list_append(Py), Prolog)
    ;   is_dict(Prolog, _Tag)
    ->  'PyDict_New'(Py),
        dict_pairs(Prolog, _, Pairs),
        maplist(py_dict_add(Py), Pairs)
    ;   type_error(python, Prolog)
    ).

prolog_atom_to_python(false, Py) :-
    !,
    'PyBool_FromLong'(0, Py).
prolog_atom_to_python(true, Py) :-
    !,
    'PyBool_FromLong'(1, Py).
prolog_atom_to_python(Atom, Py) :-
    prolog_string_to_python(Atom, Py).

prolog_string_to_python(Text, Py) :-
    sub_atom(Text, _, _, _, '\u0000'), !,
    atom_length(Text, Len),
    'PyUnicode_FromWideChar'(Text, Len, Py).
prolog_string_to_python(Text, Py) :-
    'PyUnicode_FromString'(Text, Py).

list_append(List, Prolog) :-
    prolog_to_python(Prolog, Py),
    'PyList_Append'(List, Py, RC),
    (   RC == 0
    ->  true
    ;   py_check_exception
    ).

py_dict_add(Dict, Key-Value) :-
    prolog_to_python(Value, PyValue),
    (   atom(Key)
    ->  'PyDict_SetItemString'(Dict, Key, PyValue, _Rc)
    ;   prolog_to_python(Key, PyKey),
        'PyDict_SetItem'(Dict, PyKey, PyValue, _Rc)
    ).


%!  python_to_prolog(+Python, -Prolog) is det.
%
%   Convert a Python value to a  Prolog value. Supported conversions are
%   in the table below.
%
%     | Python | Prolog            |
%     ------------------------------
%     | Bool   | `true` or `false` |
%     | Long   | integer           |
%     | Float  | float             |
%     | String | String            |
%     | List   | list              |
%     | Dict   | dict              |

python_to_prolog(Py, Value) :-
    'PyBool_Check'(Py, 1),
    !,
    'PyLong_AsLongLong'(Py, Value0),
    (   Value0 == 0
    ->  Value = false
    ;   Value = true
    ).
python_to_prolog(Py, Value) :-
    'PyLong_Check'(Py, 1),
    !,
    'PyLong_AsLongLong'(Py, Value).
python_to_prolog(Py, Value) :-
    'PyFloat_Check'(Py, 1),
    !,
    'PyFloat_AsDouble'(Py, Value).
python_to_prolog(Py, Value) :-
    'PyUnicode_Check'(Py, 1),
    !,
    'PyUnicode_AsWideCharString'(Py, Len, WString),
    c_load_string(WString, Len, Value, string, wchar_t).
python_to_prolog(Py, Value) :-
    'PyList_Check'(Py, 1),
    !,
    'PyList_Size'(Py, Len),
    py_list(0, Len, Py, Value).
python_to_prolog(Py, Value) :-
    'PyDict_Check'(Py, 1),
    !,
    py_dict_pairs(Py, Pairs),
    dict_pairs(Value, py, Pairs).
python_to_prolog(Py, _Value) :-
    throw(error(python_convert_error(python(Py)), _)).

py_list(I, Len, List, [H|T]) :-
    I < Len, !,
    'PyList_GetItem'(List, I, Item),
    python_to_prolog(Item, H),
    I2 is I+1,
    py_list(I2, Len, List, T).
py_list(Len, Len, _, []).

py_dict_pairs(PyDict, Pairs) :-
    c_alloc(KeyP, *('PyObject')),
    c_alloc(ValP, *('PyObject')),
    c_alloc(PosP, int),
    py_dict_pairs(PyDict, PosP, KeyP, ValP, Pairs).

py_dict_pairs(PyDict, PosP, KeyP, ValP, [Key-Val|T]) :-
    'PyDict_Next'(PyDict, PosP, KeyP, ValP, 1),
    !,
    c_load(KeyP, PyKey),
    c_load(ValP, PyVal),
    python_to_prolog_key(PyKey, Key),
    python_to_prolog(PyVal, Val),
    py_dict_pairs(PyDict, PosP, KeyP, ValP, T).
py_dict_pairs(_,_,_,_,[]).

python_to_prolog_key(Py, Value) :-
    'PyLong_Check'(Py, 1),
    !,
    'PyLong_AsLongLong'(Py, Value).
python_to_prolog_key(Py, Value) :-
    'PyUnicode_Check'(Py, 1),
    !,
    'PyUnicode_AsWideCharString'(Py, Len, WString),
    c_load_string(WString, Len, Value, atom, wchar_t).
python_to_prolog_key(Py, _Value) :-
    type_error(python_key, Py).


%!  py_check_exception is det.
%
%   Check whether there is an exception in  the environment and raise it
%   as a Prolog exception.
%
%   @tbd Map to Prolog
%   @tbd The exception is borrowed.  How to handle reference counts.

py_check_exception :-
    'PyErr_Occurred'(Ex),
    (   c_nil(Ex)
    ->  true
    ;   'PyErr_Clear'(),
        throw(error(python_error(Ex)))
    ).
