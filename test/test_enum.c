typedef enum dow
{ sunday,
  monday,
  tuesday,
  wednessday,
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
