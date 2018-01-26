#include <Python.h>
#include <stdio.h>

void
MyPy_DECREF(PyObject *o)
{ //fprintf(stderr, "Py_DECREF(%p)\n", o);
  Py_DECREF(o);
}

void
MyPy_INCREF(PyObject *o)
{ Py_INCREF(o);
}

