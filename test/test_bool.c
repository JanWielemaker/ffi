#include <stdbool.h>

bool gbool = false;

void
set_bool(bool v)
{ gbool = v;
}

bool
get_bool_ret(void)
{ return gbool;
}

void
get_bool_arg(bool *v)
{ *v = gbool;
}

