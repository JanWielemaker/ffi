#include <string.h>			/* get strdup() */
#include <stdlib.h>			/* get free() prototype */

typedef struct point
{ int x, y;
} point;


		 /*******************************
		 *	      OUTPUT		*
		 *******************************/

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

void
test_v_oip(int **sp)			/* output int* */
{ static int data[] = {1,2,3};

  *sp = data;
}

void
test_v_ofip(int **sp)			/* output owned int* */
{ int *data = malloc(sizeof(*data)*3);

  data[0] = 1;
  data[1] = 2;
  data[2] = 3;

  *sp = data;
}

void
test_v_ofsp(point **sp)			/* output owned struct * */
{ point *data = malloc(sizeof(*data));

  data->x = 1;
  data->y = 2;

  *sp = data;
}



		 /*******************************
		 *	      RETURN		*
		 *******************************/

const char *				/* return string */
test_s(void)
{ return "hello world";
}

const char *				/* return owned string */
test_fs(void)
{ return strdup("hello world");
}

		 /*******************************
		 *	       FREE		*
		 *******************************/

/* On Windows we cannot use free from libc as we need to call
   from the same module
*/

void
myfree(void *ptr)
{ free(ptr);
}
