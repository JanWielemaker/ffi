#include <Python.h>

void
MyPy_DECREF(PyObject *o)
{ Py_DECREF(o);
}

void
MyPy_INCREF(PyObject *o)
{ Py_INCREF(o);
}

