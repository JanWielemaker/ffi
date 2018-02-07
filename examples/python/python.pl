/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(python,
          [ py_call/1,                  % +ModuleOrObj:Call
            py_call/2,                  % +ModuleOrObj:Call, -Return

            py_object/1,                % ?Object
            py_object/2,                % ?Object, ?Class
            py_str/2,                   % +Object, -String

            py_init/0,
            py_module/2,                % +Name, -Handle
            py_function/3,              % +Module, +FuncName, -Function

            py_gil/1                    % :Goal
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

:- meta_predicate
    py_gil(0).

:- multifile user:file_search_path/2.

user:file_search_path(python_itf, Dir) :-
    source_file(py_init, File),
    file_directory_name(File, Dir).

% define macros for c_import/3.  Note that the macro is matched
% using =@=/2 (variant)
c_define(py_return, *('PyObject', 'MyPy_DECREF')).
c_define(py_object, *'PyObject').

:- c_import("//#define Py_LIMITED_API 1
	     #include \"mypython.c\"",
            [ '-lpython3.6m',
              python_itf(mypython),
              '-I/usr/include/python3.6m',
              '-I/usr/include/x86_64-linux-gnu/python3.6m'
            ],
            [ 'Py_SetProgramName'(+string(wchar_t)),
              'Py_Initialize'(),
              'PyRun_SimpleStringFlags'(string, 'PyCompilerFlags', [int]),
              'Py_FinalizeEx'([int]),

              'PyEval_InitThreads'(),
              'PyGILState_Ensure'([int]), % Actually enum ...
              'PyGILState_Release'(int),  % but we are not interested

              'PyLong_FromLongLong'(int, [py_return]),
              'PyFloat_FromDouble'(float, [py_return]),
              'PyUnicode_FromString'(+string(utf8), [py_return]),
              'PyUnicode_FromWideChar'(+string(wchar_t), +int, [py_return]),

              'MyPyLong_Check'(py_object, [-int]) as 'PyLong_Check',
              'MyPyBool_Check'(py_object, [-int]) as 'PyBool_Check',
              'MyPyFloat_Check'(py_object, [-int]) as 'PyFloat_Check',
              'MyPyUnicode_Check'(py_object, [-int]) as 'PyUnicode_Check',
              'MyPyList_Check'(py_object, [-int]) as 'PyList_Check',
              'MyPyDict_Check'(py_object, [-int]) as 'PyDict_Check',

              'PyLong_AsLongLong'(py_object, [-int]),
              'PyBool_FromLong'(int, [py_return]),
              'PyFloat_AsDouble'(py_object, [-float]),
              'PyUnicode_AsWideCharString'(py_object, -int,
                                           [*(wchar_t, 'PyMem_Free')]),

              'PyImport_Import'(py_object, [py_return]),

              'PyObject_GetAttrString'(py_object, +string,
                                       [py_return]),

              'PyTuple_New'(int, [py_return]),
              'PyTuple_SetItem'(py_object, int, py_object, [int]),

              'PyList_New'(int, [py_return]),
              'PyList_Append'(py_object, py_object, [int]),
              'PyList_GetItem'(py_object, int, [py_object]),
              'PyList_Size'(py_object, [int]),

              'PyDict_New'([py_return]),
              'PyDict_SetItemString'(py_object, string(utf8), py_object, [int]),
              'PyDict_SetItem'(py_object, py_object, py_object, [int]),
              'PyDict_Next'(py_object, *int, *py_object, *py_object, [int]),

              'PyObject_CallObject'(py_object, py_object, [py_return]),
              'PyObject_Str'(py_object, [py_return]),

              'PyErr_Occurred'([py_object]),
              'PyErr_Clear'(),

              'MyPy_DECREF'(py_object) as 'Py_DECREF',
              'MyPy_INCREF'(py_object) as 'Py_INCREF'
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
%   'PyEval_InitThreads'(),
%   set_prolog_gc_thread(false),
    asserta(py_init_done).

%!  py_module(+Name, -Module) is det.
%
%   Module is a Python object holding the executable module Name.

py_module(Name, Module) :-
    py_module_done(Name, Module0),
    !,
    Module = Module0.
py_module(Name, Module) :-
    with_mutex(python, py_gil(py_module_sync(Name, Module0))),
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
    atom(Module),
    !,
    (   py_function_done(Module, Name, Function0)
    ->  Function = Function0
    ;   with_mutex(python, py_gil(py_function_sync(Module, Name, Function0)))
    ->  Function = Function0
    ).
py_function(Obj, Name, Function) :-
    'PyObject_GetAttrString'(Obj, Name, Function).

py_function_sync(Module, Name, Function) :-
    py_function_done(Module, Name, Function),
    !.
py_function_sync(Module, Name, Function) :-
    py_module(Module, Handle),
    'PyObject_GetAttrString'(Handle, Name, Function),
    py_check_exception,
    asserta(py_function_done(Module, Name, Function)).

%!  py_call(+Call) is det.
%!  py_call(+Call, -Return) is det.
%
%   Call a Python function.  Call  is   of  the  form `Obj:Function(Arg,
%   ...)`, where `Obj` is  either  a   Python  module  (*not* the Prolog
%   module) or a Python instance, `Function` is   the name of a function
%   on `Obj` and the arguments are Prolog  values that can be translated
%   into Python values. `Function` can also be   a  plain atom, in which
%   case the named attribute is extracted.

py_call(Call) :-
    py_gil(py_call_sync(Call, _Return)).
py_call(Call, Return) :-
    py_gil(py_call_sync(Call, Return)).

py_call_sync(Obj:Attr, Return) :-
    atom(Attr),
    !,
    (   atom(Obj)
    ->  py_module(Obj, O)
    ;   O = Obj
    ),
    'PyObject_GetAttrString'(O, Attr, PyReturn),
    py_check_exception,
    python_to_prolog(PyReturn, Return).
py_call_sync(Obj:Call, Return) :-
    compound_name_arity(Call, Func, Argc),
    py_function(Obj, Func, Function),
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

%!  py_object(?Ref) is nondet.
%!  py_object(?Ref, ?ClassName) is nondet.
%
%   True if Ref is a Python object of the indicated class.

py_object(Ref) :-
    current_blob(Ref, c_ptr),
    c_typeof(Ref, struct('_object')).

py_object(Ref, ClassName) :-
    current_blob(Ref, c_ptr),
    \+ c_nil(Ref),
    c_typeof(Ref, struct('_object')),
    'PyObject_GetAttrString'(Ref, '__class__', Class),
    'PyObject_GetAttrString'(Class, '__name__', Unicode),
    'PyUnicode_Check'(Unicode, 1),
    'PyUnicode_AsWideCharString'(Unicode, Len, WString),
    c_load_string(WString, Len, ClassName, atom, wchar_t).

%!  py_str(+Obj, -String) is det.
%
%   String is the string representation of Obj.

py_str(Obj, String) :-
    'PyObject_Str'(Obj, Unicode),
    'PyUnicode_AsWideCharString'(Unicode, Len, WString),
    c_load_string(WString, Len, String, string, wchar_t).


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
python_to_prolog(Py, Py).

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
%   @tbd The exception is borrowed.  How to handle reference counts?

py_check_exception :-
    'PyErr_Occurred'(Ex),
    (   c_nil(Ex)
    ->  true
    ;   py_str(Ex, String),
        'PyErr_Clear'(),
        throw(error(python_error(String), _))
    ).

%!  py_gil(:Goal)
%
%   Call Goal with the Python GIL   (Global Interpreter Lock) held. This
%   is required for all Python interaction  if multiple (Prolog) threads
%   are involved.

py_gil(Goal) :-
    py_init,
    setup_call_cleanup(
        'PyGILState_Ensure'(GILState),
        Goal,
        'PyGILState_Release'(GILState)).


		 /*******************************
		 *           MESSAGES		*
		 *******************************/

:- multifile prolog:error_message//1.

prolog:error_message(python_error(String)) -->
    [ 'Python error: ~p'-[String] ].


		 /*******************************
		 *            PORTRAY		*
		 *******************************/

user:portray(Python) :-
    py_object(Python, Class),
    c_address(Python, Addr),
    format('<Python ~w>(0x~16r)', [Class, Addr]).
