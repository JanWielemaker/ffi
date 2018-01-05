#include <stdio.h>

typedef struct point
{ int x;
  int y;
} point;

// Not needed
// int get_point(point *p);

int
get_point(point *p)
{ // printf("p=%p\n", p);
  p->x = 42;
  p->y = 4242;

  return 0;
}

void
set_point(point *p, int x, int y)
{ p->x = x;
  p->y = y;
}

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
