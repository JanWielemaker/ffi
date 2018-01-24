typedef int (*testf)(int);

int
test(testf func, int i)
{ return (func)(i);
}
