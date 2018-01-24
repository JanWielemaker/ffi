% :- module(python, []).
:- use_module(library(ffi)).

/** <module> Embed Python

@see https://docs.python.org/3/extending/embedding.html
*/

:- c_import("#include <Python.h>",
            [ '-lpython3.6m',
              '-I/usr/include/python3.6m',
              '-I/usr/include/x86_64-linux-gnu/python3.6m'
            ],
            [ 'Py_DecodeLocale'(+string, -int, [-string(wchar_t)]),
              'Py_SetProgramName'(+string(wchar_t)),
              'Py_Initialize'(),
              'PyRun_SimpleStringFlags'(+string, +'PyCompilerFlags', [-int]),
              'Py_FinalizeEx'([-int]),

              'PyUnicode_FromString'(+string(utf8), [*'PyObject'])
            ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Issues:

  - 'Py_DecodeLocale' accepts a NULL pointer
  - 'Py_DecodeLocale' returns a pointer that must be freed using
    PyMem_RawFree()
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


py_init :-
    current_prolog_flag(os_argv, [Program|_]),
    'Py_SetProgramName'(Program),
    'Py_Initialize'().

today :-
    py_init,
    'PyRun_SimpleStringFlags'("from time import time,ctime\n\c
                               print('Today is', ctime(time()))\n",
                              null, Rc),
    writeln(Rc).
