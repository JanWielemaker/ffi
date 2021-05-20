/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017-2018, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#define PL_ARITY_AS_SIZE 1
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <stdio.h>
#include <pthread.h>

#ifdef __WINDOWS__
#define SIZEFMT "%Iu"
#else
#define SIZEFMT "%zu"
#endif

static atom_t ATOM_char;
static atom_t ATOM_short;
static atom_t ATOM_int;
static atom_t ATOM_long;
static atom_t ATOM_longlong;
static atom_t ATOM_uchar;
static atom_t ATOM_ushort;
static atom_t ATOM_uint;
static atom_t ATOM_ulong;
static atom_t ATOM_ulonglong;
static atom_t ATOM_float;
static atom_t ATOM_double;
static atom_t ATOM_pointer;
static atom_t ATOM_closure;
static atom_t ATOM_c_callback;
static atom_t ATOM_void;
static atom_t ATOM__Bool;

static atom_t ATOM_struct;
static atom_t ATOM_union;
static atom_t ATOM_enum;

static atom_t ATOM_iso_latin_1;
static atom_t ATOM_octet;
static atom_t ATOM_utf8;
static atom_t ATOM_text;
static atom_t ATOM_wchar_t;
static atom_t ATOM_char;
static atom_t ATOM_size_t;

static atom_t ATOM_atom;
static atom_t ATOM_string;
static atom_t ATOM_codes;
static atom_t ATOM_chars;
static atom_t ATOM_null;
static atom_t ATOM_star;

static functor_t FUNCTOR_struct1;
static functor_t FUNCTOR_union1;
static functor_t FUNCTOR_enum1;
static functor_t FUNCTOR_array2;
static functor_t FUNCTOR_star1;

static atom_t PTR_NULL;

static pthread_mutex_t dep_mutex = PTHREAD_MUTEX_INITIALIZER;

#define SZ_UNKNOWN (~(size_t)0)

typedef void (*freefunc)(void *ptr);

typedef struct c_dep
{ atom_t ptr;
  size_t offset;
  struct c_dep *next;
} c_dep;

typedef enum c_type
{ CT_UNKNOWN = 0,
  CT_CHAR,
  CT_UCHAR,
  CT_WCHAR_T,
  CT_SHORT,
  CT_USHORT,
  CT_INT,
  CT_UINT,
  CT_LONG,
  CT_ULONG,
  CT_SIZE_T,
  CT_LONGLONG,
  CT_ULONGLONG,
  CT_FLOAT,
  CT_DOUBLE,
  CT_CLOSURE,
  CT_POINTER,
  CT_STRUCT,
  CT_UNION,
  CT_ENUM,
  CT_VOID,
  CT_BOOL,
  CT_CALLBACK
} c_type;

#define CTF_OUTPUT	0x0001		/* Output argument */

typedef struct type_spec
{ c_type	 type;			/* CT_* */
  short		 ptrl;			/* pointer level */
  short		 flags;			/* bitwise or of CTF_* */
  atom_t	 name;			/* struct, union or enum name */
  size_t	 size;			/* Element size */
  void          *free;			/* Free function */
} type_spec;

typedef struct c_ptr
{ void *ptr;				/* the pointer */
  type_spec type;			/* element type */
  size_t count;				/* Element count behind ptr */
  c_dep *deps;				/* Dependency */
} c_ptr;


static int get_free_func(term_t t, void **func);

static int
add_dependency(c_ptr *ref, atom_t adep, size_t offset)
{ c_dep *dep = malloc(sizeof(*dep));

  if ( dep )
  { c_dep *deps;

    dep->ptr = adep;
    PL_register_atom(adep);
    deps = ref->deps;
    pthread_mutex_lock(&dep_mutex);
    dep->next = deps;
    ref->deps = dep;
    pthread_mutex_unlock(&dep_mutex);

    return TRUE;
  }

  return FALSE;
}


/*
static void
del_dependency(c_ptr *ref, atom_t adep, size_t offset)
{ c_dep **loc;

  pthread_mutex_lock(&dep_mutex);
  for(loc = &ref->deps; *loc; loc = &(*loc)->next)
  { c_dep *dep = *loc;

    if ( dep->ptr == adep && dep->offset == offset )
    { *loc = dep->next;
      PL_unregister_atom(dep->ptr);
      free(dep);
      break;
    }
  }
  pthread_mutex_unlock(&dep_mutex);
}
*/

static void
free_ptr(c_ptr *ref)
{ c_dep *dep = ref->deps;
  freefunc freef;

  if ( dep )
  { pthread_mutex_lock(&dep_mutex);
    if ( (dep=ref->deps) )
    { c_dep *next;

      for(; dep; dep=next)
      { next = dep->next;
	PL_unregister_atom(dep->ptr);
	free(dep);
      }
    }
    pthread_mutex_unlock(&dep_mutex);
  }

  if ( (freef=ref->type.free) )
  { void *p = ref->ptr;

    if ( p && __sync_bool_compare_and_swap(&ref->ptr, p, NULL) )
    { DEBUG(5, Sdprintf("free_ptr(%p)\n", p));

      (*freef)(p);
    }
  }

  if ( ref->type.name )
    PL_unregister_atom(ref->type.name);
}


static char *
qname(c_type t)
{ switch(t)
  { case CT_STRUCT: return "struct ";
    case CT_UNION:  return "union ";
    case CT_ENUM:   return "enum ";
    default:	    return "";
  }
}


static char *
pname(const c_ptr *ref, char *buf)
{ if ( ref->count == SZ_UNKNOWN )
  { return "[]";
  } else
  { sprintf(buf, "[" SIZEFMT "]", ref->count);
    return buf;
  }
}

static char *
pstars(int level, char *stars, size_t size)
{  char *o = stars;
   char *e = &stars[size-1];

   while(level-- > 0 && o < e)
     *o++ = '*';
   *o = '\0';

   return stars;
}


static const char *
tname(const type_spec *tspec)
{ switch(tspec->type)
  { case CT_CHAR:      return "char";
    case CT_UCHAR:     return "uchar";
    case CT_WCHAR_T:   return "wchar_t";
    case CT_SHORT:     return "short";
    case CT_USHORT:    return "ushort";
    case CT_INT:       return "int";
    case CT_UINT:      return "uint";
    case CT_LONG:      return "long";
    case CT_ULONG:     return "ulong";
    case CT_SIZE_T:    return "size_t";
    case CT_LONGLONG:  return "longlong";
    case CT_ULONGLONG: return "ulonglong";
    case CT_FLOAT:     return "float";
    case CT_DOUBLE:    return "double";
    case CT_CALLBACK:  return "c_callback";
    case CT_CLOSURE:   return "(*)()";
    case CT_VOID:      return "void";
    case CT_BOOL:      return "_Bool";
    case CT_STRUCT:
    case CT_UNION:
    case CT_ENUM:      return PL_atom_chars(tspec->name);
    default:	       return "???";
  }
}


static int
write_c_ptr(IOSTREAM *s, atom_t aref, int flags)
{ c_ptr *ref = PL_blob_data(aref, NULL, NULL);
  char pbuf[64];
  (void)flags;
  char stars[10];

  Sfprintf(s, "<C %s%s%s%s>(%p)",
	   qname(ref->type.type), tname(&ref->type),
	   pstars(ref->type.ptrl, stars, sizeof(stars)),
	   pname(ref, pbuf), ref->ptr);
  return TRUE;
}


static void
acquire_c_ptr(atom_t aref)
{ c_ptr *ref = PL_blob_data(aref, NULL, NULL);
  (void)ref;
}


static int
release_c_ptr(atom_t aref)
{ c_ptr *ref = PL_blob_data(aref, NULL, NULL);

  DEBUG(4, Sdprintf("Release <c>(%s,%p)\n",
		    tname(&ref->type), ref->ptr));
  free_ptr(ref);
  free(ref);

  return TRUE;
}


static int
save_c_ptr(atom_t aref, IOSTREAM *fd)
{ c_ptr *ref = PL_blob_data(aref, NULL, NULL);
  (void)fd;

  return PL_warning("Cannot save reference to <c>(%s,%p)",
		    tname(&ref->type), ref->ptr);
}


static atom_t
load_c_ptr(IOSTREAM *fd)
{ (void)fd;

  return PL_new_atom("<c>");
}


static PL_blob_t c_ptr_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_NOCOPY,
  "c_ptr",
  release_c_ptr,
  NULL,
  write_c_ptr,
  acquire_c_ptr,
  save_c_ptr,
  load_c_ptr
};


static c_ptr *
unify_ptr(term_t t, void *ptr,
	  size_t count, const type_spec *type)
{ c_ptr *ref = malloc(sizeof(*ref));

  if ( ref )
  { ref->ptr   = ptr;
    ref->count = count;
    ref->type  = *type;
    ref->deps  = NULL;

    if ( ref->type.name )
      PL_register_atom(ref->type.name);

    if ( PL_unify_blob(t, ref, sizeof(*ref), &c_ptr_blob) )
      return ref;
    return NULL;
  }

  PL_resource_error("memmory");
  return NULL;
}


static c_ptr *
get_ptr_ref(term_t t, atom_t *a)
{ atom_t ra;
  c_ptr *p;
  PL_blob_t *btype;
  int tried = 0;

retry:
  if ( PL_get_atom(t, &ra) &&
       (p=PL_blob_data(ra, NULL, &btype)) &&
       btype == &c_ptr_blob )
  { if ( a )
      *a = ra;

    return p;
  } else if ( !tried++ )
  { term_t plain = PL_new_term_ref();
    module_t m = 0;

    if ( PL_strip_module(t, &m, plain) )
    { t = plain;
      goto retry;
    }
  }

  return NULL;
}


static c_ptr *
get_ptr_ref_ex(term_t t, atom_t *a)
{ c_ptr *ref;

  if ( (ref=get_ptr_ref(t, a)) )
    return ref;

  PL_type_error("c_ptr", t);
  return NULL;
}


static int
get_ptr_direct(term_t t, void *ptrp, const type_spec *tspec)
{ c_ptr *ref;

  if ( (ref=get_ptr_ref(t, NULL)) )
  { void **ptrpp = ptrp;

    if ( tspec )
    { if ( tspec->type != ref->type.type ||
	   tspec->name != ref->type.name ||
	   tspec->ptrl != ref->type.ptrl )
      { PL_type_error(tname(tspec), t);
	return -1;
      }
    }

    *ptrpp = ref->ptr;

    return TRUE;
  }

  return FALSE;
}


static int
get_ptr(term_t t, void *ptrp, const type_spec *tspec)
{ int rc;
  atom_t null;
  int tried = 0;

retry:
  if ( (rc=get_ptr_direct(t, ptrp, tspec)) == TRUE )
    return TRUE;
  else if ( rc < 0 )
    return FALSE;
  else if ( PL_get_atom(t, &null) && null == ATOM_null )
  { void **ptrpp = ptrp;
    *ptrpp = NULL;
    return TRUE;
  } else if ( PL_is_functor(t, FUNCTOR_array2) )
  { c_ptr *ref;
    term_t arg = PL_new_term_ref();

    _PL_get_arg(2, t, arg);
    if ( (ref=get_ptr_ref(arg, NULL)) )
    { term_t list = PL_new_term_ref();
      void **ptrpp = ptrp;
      size_t offset;

      _PL_get_arg(1, t, list);
      if ( PL_get_list(list, arg, list) &&
	   PL_get_nil(list) &&
	   PL_get_size_ex(arg, &offset) )
      { *ptrpp = (char *)ref->ptr + offset*ref->type.size;
	return TRUE;
      }

      _PL_get_arg(1, t, list);
      return PL_type_error("c_offset", list);
    }

    return FALSE;
  } else if ( !tried++ )
  { term_t plain = PL_new_term_ref();
    module_t m = 0;

    if ( PL_strip_module(t, &m, plain) )
    { t = plain;
      goto retry;
    }
  }

  return PL_type_error("c_ptr", t);
}


static int
null_pointer_error(term_t ptr)
{ return PL_domain_error("non_null_pointer", ptr);
}


static int
unify_null_ptr(term_t t)
{ if ( !PTR_NULL )
  { type_spec tspec = {CT_VOID, 0, 0, 0, SZ_UNKNOWN, NULL};

    if ( unify_ptr(t, NULL, SZ_UNKNOWN, &tspec) )
    { if ( PL_get_atom(t, &PTR_NULL) )
	PL_register_atom(PTR_NULL);
      else
	assert(0);
      return TRUE;
    }

    return FALSE;
  } else
  { return PL_unify_atom(t, PTR_NULL);
  }
}


static foreign_t
c_nil(term_t t)
{ return unify_null_ptr(t);
}


static foreign_t
c_is_nil(term_t ptr)
{ c_ptr *ref;

  if ( (ref=get_ptr_ref(ptr, NULL)) &&
       ref->ptr == NULL )
    return TRUE;

  return FALSE;
}


		 /*******************************
		 *	  PROLOG BINDING	*
		 *******************************/

/* Handle one of

   - struct(Type)
   - struct(Type, Size)
   - union(Type)
   - union(Type, Size)
   - enum(Type)

   Where Type is one of `char`, `uchar`, ...

   The spec is wrapped in zero or more *(Type) terms, setting the `ptrl`
   (pointer level) field. The outermost term can be *(Type, Free),
   causing Free to be called if the pointer is released.
*/

static int
get_type(term_t t, type_spec *tspec)
{ atom_t qn;
  size_t arity;
  module_t m = NULL;
  term_t t0 = t;
  int rc;

  memset(tspec, 0, sizeof(*tspec));

  if ( !PL_strip_module(t, &m, t) )		/* module just ignored for now */
    return FALSE;

  while ( (rc=PL_get_name_arity(t, &qn, &arity)) && arity > 0 )
  { if ( qn == ATOM_star && arity <= (tspec->ptrl == 0 ? 2 : 1) )
    { if ( tspec->ptrl == 0 )
      {	t = PL_copy_term_ref(t);
	if ( arity == 2 )
	{ term_t a = PL_new_term_ref();
	  _PL_get_arg(2, t, a);
	  if ( !get_free_func(a, &tspec->free) )
	    return FALSE;
	}
      }
      _PL_get_arg(1, t, t);
      tspec->ptrl++;
      continue;
    } else
    { term_t a = PL_new_term_ref();

      _PL_get_arg(1, t, a);
      if ( PL_get_atom_ex(a, &tspec->name) )
      { if ( qn == ATOM_struct && arity <= 2 )
	  tspec->type = CT_STRUCT;
	else if ( qn == ATOM_union && arity <= 2 )
	  tspec->type = CT_UNION;
	else if ( qn == ATOM_enum && arity == 1)
	  tspec->type = CT_ENUM;
	else
	  return PL_type_error("c_type", t0);
      } else
	return PL_type_error("c_type", t0);

      if ( arity == 2 )
      { _PL_get_arg(2, t, a);
	if ( !PL_get_size_ex(a, &tspec->size) )
	  return FALSE;
      }

      return TRUE;
    }
  }

  if ( rc && arity == 0 )
  { if      ( qn == ATOM_char      ) tspec->type = CT_CHAR,
				     tspec->size = sizeof(char);
    else if ( qn == ATOM_uchar     ) tspec->type = CT_UCHAR,
				     tspec->size = sizeof(char);
    else if ( qn == ATOM_wchar_t   ) tspec->type = CT_WCHAR_T,
				     tspec->size = sizeof(wchar_t);
    else if ( qn == ATOM_short     ) tspec->type = CT_SHORT,
				     tspec->size = sizeof(short);
    else if ( qn == ATOM_ushort    ) tspec->type = CT_USHORT,
				     tspec->size = sizeof(short);
    else if ( qn == ATOM_int       ) tspec->type = CT_INT,
				     tspec->size = sizeof(int);
    else if ( qn == ATOM_uint      ) tspec->type = CT_UINT,
				     tspec->size = sizeof(int);
    else if ( qn == ATOM_long      ) tspec->type = CT_LONG,
				     tspec->size = sizeof(long);
    else if ( qn == ATOM_ulong     ) tspec->type = CT_ULONG,
				     tspec->size = sizeof(long);
    else if ( qn == ATOM_size_t    ) tspec->type = CT_SIZE_T,
				     tspec->size = sizeof(size_t);
    else if ( qn == ATOM_longlong  ) tspec->type = CT_LONGLONG,
				     tspec->size = sizeof(long long);
    else if ( qn == ATOM_ulonglong ) tspec->type = CT_ULONGLONG,
				     tspec->size = sizeof(long long);
    else if ( qn == ATOM_float     ) tspec->type = CT_FLOAT,
				     tspec->size = sizeof(float);
    else if ( qn == ATOM_double    ) tspec->type = CT_DOUBLE,
				     tspec->size = sizeof(double);
    else if ( qn == ATOM_void      ) tspec->type = CT_VOID,
				     tspec->size = 0;
    else if ( qn == ATOM_closure   ) tspec->type = CT_CLOSURE,
				     tspec->size = sizeof(void *);
    else if ( qn == ATOM_c_callback) tspec->type = CT_CALLBACK,
				     tspec->size = sizeof(void *);
    else if ( qn == ATOM__Bool     ) tspec->type = CT_BOOL,
				     tspec->size = sizeof(_Bool);
    else
      return PL_type_error("c_type", t0);

    return TRUE;
  }

  return PL_type_error("c_type", t0);
}


static int
unify_type(term_t t, const type_spec *tspec)
{ int ptrl;
  atom_t a = 0;
  functor_t f = 0;

  if ( (ptrl=tspec->ptrl) > 0 )
  { term_t copy = PL_copy_term_ref(t);

    while(ptrl-- > 0)
    { if ( !PL_unify_functor(copy, FUNCTOR_star1) ||
	   !PL_get_arg(1, copy, copy) )
	return FALSE;
    }

    t = copy;
  }

  switch(tspec->type)
  { case CT_CHAR:      a = ATOM_char;       break;
    case CT_UCHAR:     a = ATOM_uchar;      break;
    case CT_WCHAR_T:   a = ATOM_wchar_t;    break;
    case CT_SHORT:     a = ATOM_short;      break;
    case CT_USHORT:    a = ATOM_ushort;     break;
    case CT_INT:       a = ATOM_int;        break;
    case CT_UINT:      a = ATOM_uint;       break;
    case CT_LONG:      a = ATOM_long;       break;
    case CT_ULONG:     a = ATOM_ulong;      break;
    case CT_SIZE_T:    a = ATOM_size_t;     break;
    case CT_LONGLONG:  a = ATOM_longlong;   break;
    case CT_ULONGLONG: a = ATOM_ulonglong;  break;
    case CT_FLOAT:     a = ATOM_float;      break;
    case CT_BOOL:      a = ATOM__Bool;      break;
    case CT_DOUBLE:    a = ATOM_double;     break;
    case CT_STRUCT:    f = FUNCTOR_struct1; break;
    case CT_UNION:     f = FUNCTOR_union1;  break;
    case CT_ENUM:      f = FUNCTOR_enum1;   break;
    case CT_CALLBACK:  a = ATOM_c_callback; break;
    case CT_CLOSURE:   a = ATOM_closure;    break;
    default:
      assert(0);
  }

  if ( f )
    return PL_unify_term(t, PL_FUNCTOR, f, PL_ATOM, tspec->name);
  else
    return PL_unify_atom(t, a);
}



static foreign_t
c_calloc(term_t ptr, term_t type, term_t esize, term_t count)
{ type_spec tspec;
  size_t cnt;

  if ( get_type(type, &tspec) &&
       PL_get_size_ex(esize, &tspec.size) &&
       PL_get_size_ex(count, &cnt) )
  { size_t bytes = tspec.size*cnt;
    void *p = PL_malloc(bytes);

    if ( p )
    { c_ptr *ref;

      memset(p, 0, bytes);
      tspec.free = NULL;
      if ( (ref=unify_ptr(ptr, p, cnt, &tspec)) )
      { ref->type.free = PL_free;
	return TRUE;
      }
      PL_free(p);
    } else
      PL_resource_error("memory");
  }

  return FALSE;
}




static foreign_t
c_recalloc(term_t ptr, term_t count)
{ size_t cnt;
  c_ptr *ref;

  if ( PL_get_size_ex(count, &cnt) &&
       (ref=get_ptr_ref_ex(ptr, NULL)) )
  { size_t obytes;
    size_t nbytes;
    void *np;

    if ( ref->type.size == SZ_UNKNOWN )
      return PL_domain_error("sized pointer", ptr);

    obytes = ref->type.size * ref->count;
    nbytes = ref->type.size * cnt;

    if ( (np = realloc(ref->ptr, nbytes)) )
    { if ( nbytes > obytes )
	memset((char*)ref->ptr+obytes, 0, nbytes-obytes);

      ref->count = cnt;
      return TRUE;
    }

    PL_resource_error("memory");
  }

  return FALSE;
}


static foreign_t
c_free(term_t ptr)
{ c_ptr *ref;

  if ( (ref=get_ptr_ref_ex(ptr, NULL)) )
  { free_ptr(ref);
    return TRUE;
  }

  return FALSE;
}


static foreign_t
c_disown(term_t ptr)
{ c_ptr *ref;

  if ( (ref=get_ptr_ref_ex(ptr, NULL)) )
  { ref->type.free = NULL;
    return TRUE;
  }

  return FALSE;
}


static foreign_t
c_typeof(term_t ptr, term_t type)
{ c_ptr *ref;

  if ( (ref=get_ptr_ref_ex(ptr, NULL)) )
    return unify_type(type, &ref->type);

  return FALSE;
}


static foreign_t
c_dim(term_t ptr, term_t count, term_t size)
{ c_ptr *ref;

  if ( (ref=get_ptr_ref_ex(ptr, NULL)) )
  { return ( PL_unify_uint64(count, ref->count) &&
	     PL_unify_uint64(size, ref->type.size) );
  }

  return FALSE;
}


static int
valid_offset(c_ptr *ref, size_t off, size_t tsize, term_t offset)
{ if ( ref->count != SZ_UNKNOWN && ref->type.size != SZ_UNKNOWN )
  { if ( off+tsize <= ref->count*ref->type.size )
      return TRUE;
    return PL_domain_error("offset", offset);
  }

  return TRUE;
}

#define VALID(ref, off, type) valid_offset(ref, off, sizeof(type), offset)

static foreign_t
c_load(term_t ptr, term_t offset, term_t type, term_t value)
{ c_ptr *ref;
  size_t off;

  if ( (ref=get_ptr_ref_ex(ptr, NULL)) &&
       PL_get_size_ex(offset, &off) )
  { atom_t ta;
    size_t tarity;
    void *vp;

    if ( !ref->ptr )
      null_pointer_error(ptr);

    vp = (void*)((char *)ref->ptr + off);

    if ( PL_get_atom(type, &ta) )
    { if ( ta == ATOM_char )
      { const char *p = vp;
	return VALID(ref, off, char) && PL_unify_integer(value, *p);
      } else if ( ta == ATOM_uchar )
      { const unsigned char *p = vp;
	return VALID(ref, off, char) && PL_unify_integer(value, *p);
      } else if ( ta == ATOM__Bool )
      { const _Bool *p = vp;
	return VALID(ref, off, char) && PL_unify_bool(value, *p);
      } else if ( ta == ATOM_wchar_t )
      { const wchar_t *p = vp;
	return VALID(ref, off, wchar_t) && PL_unify_integer(value, *p);
      } else if ( ta == ATOM_short )
      { const short *p = vp;
	return VALID(ref, off, short) && PL_unify_integer(value, *p);
      } else if ( ta == ATOM_ushort )
      { const unsigned short *p = vp;
	return VALID(ref, off, short) && PL_unify_integer(value, *p);
      } else if ( ta == ATOM_int )
      { const int *p = vp;
	return VALID(ref, off, int) && PL_unify_integer(value, *p);
      } else if ( ta == ATOM_uint )
      { const unsigned int *p = vp;
	return VALID(ref, off, int) && PL_unify_uint64(value, *p);
      } else if ( ta == ATOM_long )
      { const long *p = vp;
	return VALID(ref, off, long) && PL_unify_integer(value, *p);
      } else if ( ta == ATOM_ulong )
      { const unsigned long *p = vp;
	return VALID(ref, off, long) && PL_unify_uint64(value, *p);
      } else if ( ta == ATOM_size_t )
      { const size_t *p = vp;
	return VALID(ref, off, size_t) && PL_unify_uint64(value, *p);
      } else if ( ta == ATOM_longlong )
      { const long long *p = vp;
	if ( !VALID(ref, off, long long ) )
	  return FALSE;
	int64_t v = (int64_t)*p;
	return PL_unify_integer(value, v);
      } else if ( ta == ATOM_ulonglong )
      { const unsigned long long *p = vp;
	if ( !VALID(ref, off, long long ) )
	  return FALSE;
	uint64_t v = (uint64_t)*p;
	return PL_unify_uint64(value, v);
      } else if ( ta == ATOM_float )
      { const float *p = vp;
	return VALID(ref, off, float) && PL_unify_float(value, *p);
      } else if ( ta == ATOM_double )
      { const double *p = vp;
	return VALID(ref, off, double) && PL_unify_float(value, *p);
      } else if ( ta == ATOM_pointer )
      { void **p = vp;
	type_spec tspec = {CT_VOID, 0, 0, 0, SZ_UNKNOWN, NULL};

	return VALID(ref, off, void*) &&
	       unify_ptr(value, *p, 1, &tspec);
      } else
	return PL_domain_error("c_type", type);
    } else if ( PL_get_name_arity(type, &ta, &tarity) )
    { term_t arg = PL_new_term_ref();

      if ( ta == ATOM_pointer && tarity == 1 )
      { type_spec tspec;

	_PL_get_arg(1, type, arg);
	if ( get_type(arg, &tspec) )
	{ void **p = vp;
	  return VALID(ref, off, void*) &&
		 unify_ptr(value, *p, SZ_UNKNOWN, &tspec);
	}
	return FALSE;
      }

      return PL_domain_error("c_type", type);
    } else
      return PL_type_error("c_type", type);
  }

  return FALSE;
}


static int
i_ptr(c_ptr *whole, term_t value, void **vp)
{ c_ptr *part;
  atom_t pa;

  if ( (part=get_ptr_ref_ex(value, &pa)) )
  { *vp = part->ptr;
    return add_dependency(whole, pa, (char*)vp - (char*)whole->ptr);
  }

  return FALSE;
}


static int
i_closure(c_ptr *whole, term_t value, void **vp)
{ if ( get_closure(value, vp) )		/* HACK: from ffi4pl.c */
  { atom_t pa;

    get_ptr_ref(value, &pa);
    return add_dependency(whole, pa, (char*)vp - (char*)whole->ptr);
  }

  return FALSE;
}


static int
PL_cvt_i_wchar(term_t t, void *vp)
{ if ( sizeof(wchar_t) == sizeof(short) ) /* assume this is optimized */
    return PL_cvt_i_short(t, vp);
  else
    return PL_cvt_i_int(t, vp);
}


static foreign_t
c_store(term_t ptr, term_t offset, term_t type, term_t value)
{ c_ptr *ref;
  size_t off;

  if ( (ref=get_ptr_ref_ex(ptr, NULL)) &&
       PL_get_size_ex(offset, &off) )
  { atom_t ta;
    void *vp = (void*)((char *)ref->ptr + off);

    if ( PL_get_atom(type, &ta) )
    {      if ( ta == ATOM_char )
	return VALID(ref, off, char) && PL_cvt_i_char(value, vp);
      else if ( ta == ATOM_uchar )
	return VALID(ref, off, char) && PL_cvt_i_uchar(value, vp);
      else if ( ta == ATOM__Bool )
	return VALID(ref, off, char) && PL_cvt_i_bool(value, vp);
      else if ( ta == ATOM_wchar_t )
	return VALID(ref, off, wchar_t) && PL_cvt_i_wchar(value, vp);
      else if ( ta == ATOM_short )
	return VALID(ref, off, short) && PL_cvt_i_short(value, vp);
      else if ( ta == ATOM_ushort )
	return VALID(ref, off, short) && PL_cvt_i_ushort(value, vp);
      else if ( ta == ATOM_int )
	return VALID(ref, off, int) && PL_cvt_i_int(value, vp);
      else if ( ta == ATOM_uint )
	return VALID(ref, off, int) && PL_cvt_i_uint(value, vp);
      else if ( ta == ATOM_long )
	return VALID(ref, off, long) && PL_cvt_i_long(value, vp);
      else if ( ta == ATOM_ulong )
	return VALID(ref, off, long) && PL_cvt_i_ulong(value, vp);
      else if ( ta == ATOM_size_t )
	return VALID(ref, off, size_t) && PL_cvt_i_size_t(value, vp);
      else if ( ta == ATOM_longlong )
	return VALID(ref, off, long long) && PL_cvt_i_int64(value, vp);
      else if ( ta == ATOM_ulonglong )
	return VALID(ref, off, long long) && PL_cvt_i_uint64(value, vp);
      else if ( ta == ATOM_float )
	return VALID(ref, off, float) && PL_cvt_i_single(value, vp);
      else if ( ta == ATOM_double )
	return VALID(ref, off, double) && PL_cvt_i_float(value, vp);
      else if ( ta == ATOM_pointer )
	return VALID(ref, off, void*) && i_ptr(ref, value, vp);
      else if ( ta == ATOM_closure )
	return VALID(ref, off, void*) && i_closure(ref, value, vp);
      else return PL_domain_error("c_type", type);
    }
  }

  return FALSE;
}


static int
get_count_or_unknown(term_t t, size_t *sz)
{ if ( PL_is_variable(t) )
  { *sz = SZ_UNKNOWN;
    return TRUE;
  }

  return PL_get_size_ex(t, sz);
}


static foreign_t
c_offset(term_t ptr0, term_t offset,
	 term_t type, term_t size, term_t count,
	 term_t ptr)
{ c_ptr *ref;
  atom_t ptra;
  size_t off;
  size_t cnt;
  type_spec tspec;

  if ( (ref=get_ptr_ref_ex(ptr0, &ptra)) &&
       PL_get_size_ex(offset, &off) &&
       get_count_or_unknown(count, &cnt) &&
       get_type(type, &tspec) &&
       PL_get_size_ex(size, &tspec.size) )
  { void *vp = (void*)((char *)ref->ptr + off);

    if ( unify_ptr(ptr, vp, cnt, &tspec) )
    { c_ptr *ref2 = get_ptr_ref_ex(ptr, NULL);
      return add_dependency(ref2, ptra, (size_t)-1);
    }
  }

  return FALSE;
}


static foreign_t
c_address(term_t ptr, term_t address)
{ c_ptr *ref;

  if ( (ref=get_ptr_ref_ex(ptr, NULL)) )
  { return PL_unify_integer(address, (intptr_t)ref->ptr);
  }

  return FALSE;
}


static foreign_t
c_sizeof(term_t type, term_t bytes)
{ atom_t ta;
  int sz;

  if ( PL_get_atom(type, &ta) )
  {      if ( ta == ATOM_char )      sz = sizeof(char);
    else if ( ta == ATOM_uchar )     sz = sizeof(unsigned char);
    else if ( ta == ATOM__Bool )     sz = sizeof(_Bool);
    else if ( ta == ATOM_short )     sz = sizeof(short);
    else if ( ta == ATOM_ushort )    sz = sizeof(unsigned short);
    else if ( ta == ATOM_int )       sz = sizeof(int);
    else if ( ta == ATOM_uint )      sz = sizeof(unsigned int);
    else if ( ta == ATOM_long )      sz = sizeof(long);
    else if ( ta == ATOM_ulong )     sz = sizeof(unsigned long);
    else if ( ta == ATOM_longlong )  sz = sizeof(long long);
    else if ( ta == ATOM_ulonglong ) sz = sizeof(unsigned long long);
    else if ( ta == ATOM_float )     sz = sizeof(float);
    else if ( ta == ATOM_double )    sz = sizeof(double);
    else if ( ta == ATOM_pointer )   sz = sizeof(void*);
    else if ( ta == ATOM_wchar_t )   sz = sizeof(wchar_t);
    else if ( ta == ATOM_size_t )    sz = sizeof(size_t);
    else return FALSE;

    return PL_unify_integer(bytes, sz);
  }

  return FALSE;
}


static foreign_t
c_alignof(term_t type, term_t bytes)
{ atom_t ta;
  int sz;

  if ( PL_get_atom(type, &ta) )
  {      if ( ta == ATOM_char )      sz = __alignof__(char);
    else if ( ta == ATOM_uchar )     sz = __alignof__(unsigned char);
    else if ( ta == ATOM__Bool )     sz = __alignof__(_Bool);
    else if ( ta == ATOM_short )     sz = __alignof__(short);
    else if ( ta == ATOM_ushort )    sz = __alignof__(unsigned short);
    else if ( ta == ATOM_int )       sz = __alignof__(int);
    else if ( ta == ATOM_uint )      sz = __alignof__(unsigned int);
    else if ( ta == ATOM_long )      sz = __alignof__(long);
    else if ( ta == ATOM_ulong )     sz = __alignof__(unsigned long);
    else if ( ta == ATOM_size_t )    sz = __alignof__(size_t);
    else if ( ta == ATOM_longlong )  sz = __alignof__(long long);
    else if ( ta == ATOM_ulonglong ) sz = __alignof__(unsigned long long);
    else if ( ta == ATOM_float )     sz = __alignof__(float);
    else if ( ta == ATOM_double )    sz = __alignof__(double);
    else if ( ta == ATOM_pointer )   sz = __alignof__(void*);
    else if ( ta == ATOM_wchar_t )   sz = __alignof__(wchar_t);
    else return FALSE;

    return PL_unify_integer(bytes, sz);
  }

  return FALSE;
}


static foreign_t
c_alloc_string(term_t ptr, term_t data, term_t encoding)
{ atom_t aenc;
  int flags = CVT_EXCEPTION|BUF_MALLOC|CVT_ATOM|CVT_STRING|CVT_LIST|CVT_INTEGER;
  char *s;
  size_t len;

  if ( !PL_get_atom_ex(encoding, &aenc) )
    return FALSE;

  if ( aenc == ATOM_iso_latin_1 )
  { flags |= REP_ISO_LATIN_1;
  } else if ( aenc == ATOM_utf8 )
  { flags |= REP_UTF8;
  } else if ( aenc == ATOM_octet )
  { flags |= REP_ISO_LATIN_1;
  } else if ( aenc == ATOM_text )
  { flags |= REP_MB;
  } else
  { if ( aenc == ATOM_wchar_t )
    { pl_wchar_t *ws;

      if ( PL_get_wchars(data, &len, &ws, flags) )
      { type_spec tspec = {CT_WCHAR_T, 0, 0, 0, sizeof(wchar_t), PL_free};

	if ( unify_ptr(ptr, ws, (len+1), &tspec) )
	  return TRUE;
	PL_free(ws);
      }
    } else
      return PL_domain_error("encoding", encoding);
  }

  if ( PL_get_nchars(data, &len, &s, flags) )
  { type_spec tspec = {CT_CHAR, 0, 0, 0, sizeof(char), PL_free};

    if ( unify_ptr(ptr, s, len+1, &tspec) )
      return TRUE;
    PL_free(s);
  }

  return FALSE;
}


static foreign_t
c_load_string5(term_t ptr, term_t len, term_t data, term_t type, term_t encoding)
{ size_t clen;
  c_ptr *ref;

  if ( len )
  { if ( !PL_get_size_ex(len, &clen) )
      return FALSE;
  } else
  { clen = (size_t)-1;
  }

  if ( (ref=get_ptr_ref_ex(ptr, NULL)) )
  { atom_t aenc, atype;
    int flags = 0;

    if ( ref->ptr == NULL )
      return null_pointer_error(ptr);

    if ( !PL_get_atom_ex(encoding, &aenc) ||
	 !PL_get_atom_ex(type, &atype) )
      return FALSE;

    if ( atype == ATOM_atom )
      flags |= PL_ATOM;
    else if ( atype == ATOM_string )
      flags |= PL_STRING;
    else if ( atype == ATOM_codes )
      flags |= PL_CODE_LIST;
    else if ( atype == ATOM_chars )
      flags |= PL_CHAR_LIST;
    else
      return PL_domain_error("text_type", type);

    if ( aenc == ATOM_wchar_t )
      return PL_unify_wchars(data, flags, clen, ref->ptr);

    if ( aenc == ATOM_iso_latin_1 )
    { flags |= REP_ISO_LATIN_1;
    } else if ( aenc == ATOM_octet )
    { flags |= REP_ISO_LATIN_1;
    } else if ( aenc == ATOM_utf8 )
    { flags |= REP_UTF8;
    } else if ( aenc == ATOM_text )
    { flags |= REP_MB;
    } else
      return PL_domain_error("encoding", encoding);

    return PL_unify_chars(data, flags, clen, ref->ptr);
  }

  return FALSE;
}

static foreign_t
c_load_string4(term_t ptr, term_t data, term_t type, term_t encoding)
{ return c_load_string5(ptr, 0, data, type, encoding);
}


#define MKATOM(n) \
        ATOM_ ## n = PL_new_atom(#n)
#define MKFUNCTOR(n,a) \
        FUNCTOR_ ## n ## a = PL_new_functor(PL_new_atom(#n), a)

static void
install_c_memory(void)
{ MKATOM(char);
  MKATOM(short);
  MKATOM(int);
  MKATOM(long);
  MKATOM(longlong);
  MKATOM(uchar);
  MKATOM(ushort);
  MKATOM(uint);
  MKATOM(ulong);
  MKATOM(ulonglong);
  MKATOM(float);
  MKATOM(double);
  MKATOM(pointer);
  MKATOM(closure);
  MKATOM(c_callback);
  MKATOM(void);
  MKATOM(_Bool);
  MKATOM(iso_latin_1);
  MKATOM(octet);
  MKATOM(utf8);
  MKATOM(text);
  MKATOM(wchar_t);
  MKATOM(size_t);
  MKATOM(char);
  MKATOM(atom);
  MKATOM(string);
  MKATOM(codes);
  MKATOM(chars);
  MKATOM(struct);
  MKATOM(union);
  MKATOM(enum);
  MKATOM(null);
  ATOM_star = PL_new_atom("*");

  MKFUNCTOR(struct, 1);
  MKFUNCTOR(union, 1);
  MKFUNCTOR(enum, 1);
  FUNCTOR_array2 = PL_new_functor(ATOM_nil, 2);
  FUNCTOR_star1  = PL_new_functor(ATOM_star, 1);

  PL_register_foreign("c_calloc",	4, c_calloc,	   0);
  PL_register_foreign("c_recalloc",	2, c_recalloc,	   0);
  PL_register_foreign("c_free",		1, c_free,	   0);
  PL_register_foreign("c_disown",	1, c_disown,	   0);
  PL_register_foreign("c_load",		4, c_load,	   0);
  PL_register_foreign("c_store",	4, c_store,	   0);
  PL_register_foreign("c_offset",	6, c_offset,	   0);
  PL_register_foreign("c_address",	2, c_address,	   0);
  PL_register_foreign("c_dim",	        3, c_dim,	   0);
  PL_register_foreign("c_typeof",	2, c_typeof,	   0);
  PL_register_foreign("c_sizeof",	2, c_sizeof,	   0);
  PL_register_foreign("c_alignof",	2, c_alignof,	   0);
  PL_register_foreign("c_alloc_string",	3, c_alloc_string, 0);
  PL_register_foreign("c_load_string",	4, c_load_string4, 0);
  PL_register_foreign("c_load_string",	5, c_load_string5, 0);
  PL_register_foreign("c_nil",		1, c_nil,          0);
  PL_register_foreign("c_is_nil",	1, c_is_nil,       0);
}
