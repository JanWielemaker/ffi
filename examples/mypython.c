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

int
MyPyLong_Check(PyObject *o)
{ return PyLong_Check(o);
}

int
MyPyFloat_Check(PyObject *o)
{ return PyFloat_Check(o);
}

int
MyPyUnicode_Check(PyObject *o)
{ return PyUnicode_Check(o);
}
