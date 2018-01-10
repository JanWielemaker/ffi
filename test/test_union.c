typedef union convert
{ double d;
  char   s[sizeof(double)];
} convert;

void
set_d(convert *c, double d)
{ c->d = d;
}
