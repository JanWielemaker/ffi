typedef long rtype;
typedef rtype (*testf)(int);

rtype
test(testf func, int i)
{ return (func)(i);
}
