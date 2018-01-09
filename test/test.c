#include <stdlib.h>

typedef int dim;

typedef struct point
{ dim x;
  dim y;
} point;

int
get_point(point *p)
{ p->x = 42;
  p->y = 4242;

  return 0;
}

void
set_point(point *p, dim x, dim y)
{ p->x = x;
  p->y = y;
}

typedef struct points
{ struct points *next;
  point pt;
} points;

static points *list = (points*)0;

void
add_point(dim x, dim y)
{ points *p = malloc(sizeof(*p));

  p->pt.x = x;
  p->pt.y = y;
  p->next = list;
  list = p;
}

points *
get_points(void)
{ return list;
}


		 /*******************************
		 *	     ENUMS		*
		 *******************************/

typedef enum dow
{ sunday,
  monday,
  tuesday,
  wednessday = 3*4,
  thursday,
  friday,
  saturday
} dow;

void
set_dow(dow *ptr, enum dow d)
{ *ptr = d;
}

enum dow
get_dow(dow *ptr)
{ return *ptr;
}

void
get_dow2(dow *ptr, dow *out)
{ *out = *ptr;
}
