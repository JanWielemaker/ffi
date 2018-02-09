#include <string.h>			/* get strdup() */
#include <stdlib.h>			/* get free() prototype */

void
test_v_oi(int *i)			/* output integer */
{ *i = 42;
}

void
test_v_os(char **sp)			/* output string */
{ *sp = "hello world";
}

void
test_v_ofs(char **sp)			/* output owned string */
{ *sp = strdup("hello world");
}

const char *
test_s(void)
{ return "hello world";
}

const char *
test_fs(void)
{ return strdup("hello world");
}

