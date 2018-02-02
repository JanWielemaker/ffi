typedef long rtype;
typedef rtype (*testf)(int);

typedef struct funcs
{ double (*mul_di)(double, int);
  int	times;
} funcs;


rtype
test_fi(testf func, int i)
{ return (func)(i);
}

double
test_fstruct(funcs *f, double d)
{ return (*f->mul_di)(d, f->times);
}
