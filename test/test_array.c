#include <stddef.h>
#include <string.h>
#include <stdio.h>

typedef int    *int_array;
typedef int    **int_arrayarray;
typedef double *dbl_array;
typedef float  *flt_array;

struct struct_s {
   int fa;
   float fb;
};

enum enum_e {
   ea=5,
   eb=8,
   ec=1
};

union union_u {
   int ua;
   char ub[4];
};

typedef struct struct_s *structptr_array;


const float  farr[]          = { 1.0f, 0.0f, 1.0f };
const double darr[]          = { 3.0, 2.0, 5.0 };
const int    iarr[]          = { 3, 1, 0, 2, 5 };
const struct struct_s sarr[] = { { 1,2 }, { 3,5 }, { 7,8 } };
const enum   enum_e   earr[] = {  ec ,  eb ,  ea  };
const union  union_u  uarr[] = {  55 ,  33,  88  };

/*
const float  farrarr[][3]         = { { 1.0f, 0.0f, 1.0f }, { 3.0f, 1.0f, 3.0f } };
const double darrarr[][3]         = { { 3.0, 2.0, 5.0 },    { 5.0, 2.0, 3.0 }    };
const int    iarrarr[][3]         = { { 3, 1, 0 },          { 5, 3, 2 }          };
const struct struct_s sarrarr[][2]= { {{ 1,2 }, {3,5}},    {{ 7,8}, {3, 2}}      };
const enum   enum_e   earrarr[][3]= { { ec , eb, ea },     { ea, ec, eb }        };
const union  union_u  uarrarr[][3]= { { 55 , 33, 88 },     { 35, 58, 32 }        };
*/

// Lists
int_array
test_intarr()
{ return (int_array) iarr;
}

void
test_fltarr(flt_array out,size_t n)
{  memcpy(out,farr,sizeof(float)*n);
}

structptr_array
test_structarr()
{  return (structptr_array) sarr;
}

enum enum_e*
test_enumarr()
//{  printf("earr %p   %p   %p\n",earr,earr+1,earr+2);
{  return (enum enum_e*) earr;
}

union union_u*
test_unionarr()
{  return (union union_u*) uarr;
}



/*
// List of lists
int_array*
test_intarrarr()
{ return (int_array*) iarrarr;
}

void
test_fltarrarr(flt_array* out,size_t n)
{  memcpy(out,farrarr,sizeof(float)*n);
}

structptr_array*
test_structarrarr()
{  return (structptr_array*) sarrarr;
}

enum enum_e**
test_enumarrarr()
//{  printf("earr %p   %p   %p\n",earr,earr+1,earr+2);
{  return (enum enum_e**) earr;
}

union union_u**
test_unionarrarr()
{  return (union union_u**) uarr;
}
*/
