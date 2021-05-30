typedef int    *int_array;
typedef int    **int_arrayarray;
typedef double *dbl_array;
typedef float  *flt_array;

const float  farr[] = { 1.0f, 0.0f, 1.0f };
const double darr[] = { 3.0, 2.0, 5.0 };
const int    iarr[] = { 3, 1, 3, 2, 5 };

int_array
test_intarray()
{ return (int_array) iarr;
}

void
test_fltarray(flt_array out)
{  out = (flt_array) farr;
}
