#include <Python.h>

void
MyPy_INCREF(PyObject *o)
{ Py_INCREF(o);
}

typedef struct py_delayed
{ PyObject *obj;
  struct py_delayed *next;
} py_delayed;

static int scheduled = 0;
static py_delayed *delayed;

static int
delayed_decref(void *ptr)
{ py_delayed *d = delayed;

  scheduled = 0;

  if ( __sync_bool_compare_and_swap(&delayed, d, NULL) )
  { py_delayed *n;

    for(; d; d=n)
    { n = d->next;
      Py_DECREF(d->obj);
      free(d);
    }
  }

  return 0;
}

void
MyPy_DECREF(PyObject *o)
{ if ( PyGILState_Check() )
  { Py_DECREF(o);
  } else
  { py_delayed *d = malloc(sizeof(*d));
    py_delayed *old;

    d->obj = o;
    do
    { old = delayed;
      d->next = old;
    } while ( !__sync_bool_compare_and_swap(&delayed, old, d) );

    if ( __sync_bool_compare_and_swap(&scheduled, 0, 1) )
      Py_AddPendingCall(delayed_decref, NULL);
  }
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
MyPySequence_Check(PyObject *o)
{ return PySequence_Check(o);
}

int
MyPyDict_Check(PyObject *o)
{ return PyDict_Check(o);
}

