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
test_v_Os(char **sp)			/* output owned string */
{ *sp = strdup("hello world");
}

