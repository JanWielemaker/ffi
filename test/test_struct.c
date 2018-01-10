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

void
clear_points(void)
{ points *p = list, *next;

  list = NULL;

  for(; p; p = next)
  { next = p->next;
    free(p);
  }
}
