typedef long rtype;
typedef int mytype;

typedef rtype (*testf)(int);

// to test typedefs in callback/closure parameters
typedef rtype (*testf_param_typedef)(mytype);

typedef struct funcs
{ double (*mul_di)(double, int);
  int	times;
} funcs;


rtype
test_fi(testf func, int i)
{ return (func)(i);
}

rtype
test_fi_param_typedef(testf_param_typedef func, mytype i)
{ return (func)(i);
}

double
test_fstruct(funcs *f, double d)
{ return (*f->mul_di)(d, f->times);
}
