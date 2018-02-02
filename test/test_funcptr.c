typedef long rtype;
typedef rtype (*testf)(int);

typedef struct funcs
{ double (*mul_di)(double, int);
} funcs;


rtype
test(testf func, int i)
{ return (func)(i);
}

double
test_fstruct(funcs *f, double d, int i)
{ return (*f->mul_di)(d, i);
}
