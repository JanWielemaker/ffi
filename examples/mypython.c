#include <Python.h>

void
MyPy_DECREF(PyObject *o)
{ PyGILState_STATE gstate;
  gstate = PyGILState_Ensure();
  Py_DECREF(o);
  PyGILState_Release(gstate);
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
MyPyBool_Check(PyObject *o)
{ return PyBool_Check(o);
}

int
MyPyFloat_Check(PyObject *o)
{ return PyFloat_Check(o);
}

int
MyPyUnicode_Check(PyObject *o)
{ return PyUnicode_Check(o);
}

int
MyPyList_Check(PyObject *o)
{ return PyList_Check(o);
}

int
MyPyDict_Check(PyObject *o)
{ return PyDict_Check(o);
}

