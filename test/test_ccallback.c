typedef long rtype;
typedef rtype (*callback_t)(int);

// struct used to test storing callback
typedef struct funcs
{ int (*cb)(int);
  int	value;
} funcs;


// test callback function
rtype
ctwice(int in) {
    return in*2;
}

// default callback function
rtype
default_callback(int in) {
    return in*3;
}


// to test pass C callback from prolog
rtype
test_passcallback(callback_t func, int i)
{ return func == 0 ? default_callback(i):(func)(i);
}

// to test C callback in struct
int
test_instruct(funcs *f)
{ return (*f->cb)(f->value);
}
