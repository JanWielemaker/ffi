# Dynamic calling C from Prolog

This package deals with calling C   functions from shared objects (DLLs)
from Prolog without writing wrappers. Currently   there  are two options
for calling C:

  - Use SWI-Prolog's native C interface. This requires defining
    a C function that takes Prolog specific arguments (`term_t`),
    performs the necessary data transformation, calls the target
    and transform the result.  The functions are registered using
    PL_register_foreign() calls from a single `void entry(void)`
    function that is called dynamically.

  - Use library(qpforeign).  That allows for a rather simple and
    old fashioned declaration of the C functions you want to use
    from Prolog.  It generates the above wrappers for you and
    optionally compiles and links them.

Currently, there are several  libraries  that   allows  for  creating an
argument vector for a C  function   dynamically  and  call the function.
These however only deal with the  C   basic  types: the various integers
types, floats and pointers. Notable  pointers are problematic. Typically
these are pointers to a value or  an   array  of  values and they may be
input, output or both. The values   may be simple primitives, structures
or unions.


