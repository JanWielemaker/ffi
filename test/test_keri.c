#include <assert.h>
#include <string.h>
#include <stdlib.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
* Parameter direction

Along with being able to understand the type of a parameter, a marshaller naturally needs to understand the direction (input/output) of a parameter:

1. return parameters. (Prolog output)
2. 'in' parameters. (Prolog inputs)
3. 'out' parameters. (Prolog outputs)
4. 'in-out' parameters. (A Prolog input+output mapped to a single function parameter)

(return parameters are typically just a special case of an 'out' parameter).

Some example functions covering these cases follow:
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* return value */
int
test_int_return(void)
{
  return 42;
}

/* in parameter */
void
test_int_in(int v)
{
  assert(v == 42);
}

/* out parameter */
void
test_int_out(int *v)
{
  *v = 42;
}

/* in-out parameter */
void
test_int_in_out(int *v)
{
  assert(*v == 42);
  *v = 24;
}


/*
Special handling may be required to handle the special case where a
pointer type can be NULL. For example:
*/

/* return value; NULL */
char*
test_null_return(void)
{
  return NULL;
}

/* in parameter; NULL */
void
test_null_in(char *v)
{
  assert(v == NULL);
}

/* out parameter; NULL */
void
test_null_out(char **v)
{
  *v = NULL;
}

/* in-out parameter; NULL */
void
test_null_in_out(char **v)
{
  assert(*v == NULL);
  *v = NULL;
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
* memory management

A marshaller should be able to track who "owns" a parameter and is thus responsible for eventually freeing resources associated with that parameter. When a parameter passes between Prolog <-> C, there are three types of transfer-of-ownership:

1. no transfer of ownership
   * 'in' parameters must be freed from Prolog
   * 'out' parameters must not be freed from Prolog
   * the 'in' component of 'in-out' parameters must be freed from Prolog

2. full transfer of ownership
   * 'in' parameters must not be freed from Prolog
   * 'out' parameters must be freed from Prolog
   * the 'out' component of 'in-out' parameters must be freed from Prolog

3. only transfer ownership of the container of a complex parameter (array, struct etc)
   * the member elements of 'in' parameters must be freed from Prolog
   * the container (but not the member elements) of 'out' parameters must be freed from Prolog
   * the member elements of the 'in' component, and the container (but not the member elements) of the 'out' component, of 'in-out' parameters must be freed from Prolog

(As mentioned earlier, parameters/member-elements are freed from Prolog either via AGC or with explicit calls to c_free/1). Some example functions covering these cases follow:
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* in parameter; do not transfer ownership */
void
test_transfer_none_in(char *v)
{
  assert(strcmp("foo", v)==0);
}

/* in parameter; full transfer of ownership */
void
test_transfer_full_in(char *v)
{
  assert(strcmp("foo", v)==0);
  free(v);
}

/* out parameter; do not transfer ownership */
void
test_transfer_none_out(char **v)
{
  *v = "foo";
}

/* out parameter; full transfer of ownership */
void
test_transfer_full_out(char **v)
{
  *v = strdup("foo");
}

/* in-out parameter; do not transfer ownership */
void
test_transfer_none_in_out(char **v)
{
  assert(strcmp("foo", *v)==0);
  *v = "bar";
}

/* in-out parameter; full transfer of ownership */
void
test_transfer_full_in_out(char **v)
{
  assert(strcmp("foo", *v)==0);
  free(*v);
  *v = strdup("bar");
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Container types (e.g. arrays or structs) can be a bit more complicated
to deal with as the parameter may have a different ownership than the
elements/field-values that make up the parameter. For example:
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* in parameter; do not transfer ownership */
void
tests_array_transfer_none_in (char **array)
{
  assert(strcmp("foo", array[0])==0);
  assert(strcmp("bar", array[1])==0);
}

/* in parameter; full transfer of ownership */
void
tests_array_transfer_full_in (char **array)
{
  assert(strcmp("foo", array[0])==0);
  assert(strcmp("bar", array[1])==0);
  free(array[0]);
  free(array[1]);
  free(array);
}

/* in parameter; transfer container of array */
void
test_array_transfer_container_in(char **array)
{
  assert(strcmp("foo", array[0])==0);
  assert(strcmp("bar", array[1])==0);
  free(array);
}

/* out parameter; do not transfer ownership */
void
test_array_transfer_none_out(char ***array)
{
  static char *array_[] = { "foo", "bar" };
  *array = array_;
}

/* out parameter; full transfer of ownership */
void
test_array_transfer_full_out(char ***array)
{
  *array = malloc(2 * sizeof(char*));
  *array[0] = strdup("foo");
  *array[1] = strdup("bar");
}

/* out parameter; transfer container of array */
void
test_array_transfer_container_out(char ***array)
{
  *array = malloc(2 * sizeof(char*));
  *array[0] = "foo";
  *array[1] = "bar";
}

/* in-out parameter; do not transfer ownership */
void
test_array_transfer_none_in_out(char ***array)
{
  static char *array_[] = { "FOO", "BAR" };
  assert(strcmp("foo", *array[0])==0);
  assert(strcmp("bar", *array[1])==0);
  *array = array_;
}

/* in-out parameter; full transfer of ownership */
void
test_array_transfer_full_in_out(char ***array)
{
  assert(strcmp("foo", *array[0])==0);
  assert(strcmp("bar", *array[1])==0);
  free(*array[0]);
  free(*array[1]);
  free(*array);
  *array = malloc(2 * sizeof(char*));
  *array[0] = strdup("FOO");
  *array[1] = strdup("BAR");
}

/* in-out parameter; transfer container of array */
void
test_array_transfer_container_in_out(char ***array)
{
  assert(strcmp("foo", *array[0])==0);
  assert(strcmp("bar", *array[1])==0);
  free(*array);
  *array = malloc(2 * sizeof(char*));
  *array[0] = "FOO";
  *array[1] = "BAR";
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A marshaller should (ideally) also be able to handle the case where a C
function is 'broken' and does not allocate an out parameter that is
described as having full transfer of ownership.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* out parameter; full transfer of ownership */
void
test_transfer_dangling_out(char **v)
{
}
