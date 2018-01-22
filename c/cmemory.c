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
static atom_t ATOM_void;

static atom_t ATOM_struct;
static atom_t ATOM_union;
static atom_t ATOM_enum;

static atom_t ATOM_iso_latin_1;
static atom_t ATOM_utf8;
static atom_t ATOM_text;
static atom_t ATOM_wchar_t;
static atom_t ATOM_char;

static atom_t ATOM_atom;
static atom_t ATOM_string;
static atom_t ATOM_codes;
static atom_t ATOM_chars;

static functor_t FUNCTOR_struct1;
static functor_t FUNCTOR_union1;
static functor_t FUNCTOR_enum1;
static functor_t FUNCTOR_array2;

static pthread_mutex_t dep_mutex = PTHREAD_MUTEX_INITIALIZER;

#define SZ_UNKNOWN (~(size_t)0)

typedef void (*freefunc)(void *ptr);

typedef struct c_dep
{ atom_t ptr;
  size_t offset;
  struct c_dep *next;
} c_dep;


typedef enum
{ Q_PLAIN = 0,
  Q_STRUCT,
  Q_UNION,
  Q_ENUM
} type_qualifier;


typedef struct c_ptr
{ void *ptr;				/* the pointer */
  atom_t type;				/* Its type */
  type_qualifier qual;			/* Type qualifier */
  size_t size;				/* Element size behind ptr */
  size_t count;				/* Element count behind ptr */
  freefunc free;			/* Its free function */
  c_dep *deps;				/* Dependency */
} c_ptr;


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

  if ( ref->free )
  { void *p = ref->ptr;

    if ( __sync_bool_compare_and_swap(&ref->ptr, p, NULL) )
    { DEBUG(5, Sdprintf("free_ptr(%p)\n", p));

      (*ref->free)(p);
    }
  }

  PL_unregister_atom(ref->type);
}


static char *
qname(type_qualifier q)
{ switch(q)
  { case Q_PLAIN:  return "";
    case Q_STRUCT: return "struct ";
    case Q_UNION:  return "union ";
    case Q_ENUM:   return "enum ";
    default:	   return "?";
  }
}


static char *
pname(const c_ptr *ref, char *buf)
{ if ( ref->count == SZ_UNKNOWN )
  { return "*";
  } else
  { sprintf(buf, "[%zd]", ref->count);
    return buf;
  }
}


static int
write_c_ptr(IOSTREAM *s, atom_t aref, int flags)
{ c_ptr *ref = PL_blob_data(aref, NULL, NULL);
  char pbuf[64];
  (void)flags;

  Sfprintf(s, "<C %s%s%s>(%p)",
	   qname(ref->qual), PL_atom_chars(ref->type), pname(ref, pbuf), ref->ptr);
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
		    PL_atom_chars(ref->type), ref->ptr));
  free_ptr(ref);
  free(ref);

  return TRUE;
}


static int
save_c_ptr(atom_t aref, IOSTREAM *fd)
{ c_ptr *ref = PL_blob_data(aref, NULL, NULL);
  (void)fd;

  return PL_warning("Cannot save reference to <c>(%s,%p)",
		    PL_atom_chars(ref->type), ref->ptr);
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


static int
unify_ptr(term_t t, void *ptr,
	  size_t count, size_t size,
	  atom_t type, type_qualifier qual,
	  freefunc free)
{ c_ptr *ref = malloc(sizeof(*ref));

  if ( ref )
  { ref->ptr   = ptr;
    ref->type  = type;
    ref->qual  = qual;
    ref->count = count;
    ref->size  = size;
    ref->free  = free;
    ref->deps  = NULL;

    PL_register_atom(ref->type);
    return PL_unify_blob(t, ref, sizeof(*ref), &c_ptr_blob);
  }

  return PL_resource_error("memmory");
}


static c_ptr *
get_ptr_ref(term_t t, atom_t *a)
{ atom_t ra;
  c_ptr *p;
  PL_blob_t *btype;

  if ( PL_get_atom(t, &ra) &&
       (p=PL_blob_data(ra, NULL, &btype)) &&
       btype == &c_ptr_blob )
  { if ( a )
      *a = ra;

    return p;
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
get_ptr_direct(term_t t, void *ptrp, atom_t ptrtype)
{ c_ptr *ref;

  if ( (ref=get_ptr_ref(t, NULL)) )
  { void **ptrpp = ptrp;

    if ( ptrtype && ptrtype != ref->type )
    { PL_type_error(PL_atom_chars(ref->type), t);
      return -1;
    }

    *ptrpp = ref->ptr;

    return TRUE;
  }

  return FALSE;
}


static int
get_ptr(term_t t, void *ptrp, atom_t ptrtype)
{ int rc;

  if ( (rc=get_ptr_direct(t, ptrp, ptrtype)) == TRUE )
    return TRUE;
  else if ( rc < 0 )
    return FALSE;
  else if ( PL_is_functor(t, FUNCTOR_array2) )
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
      { *ptrpp = (char *)ref->ptr + offset*ref->size;
	return TRUE;
      }

      _PL_get_arg(1, t, list);
      return PL_type_error("c_offset", list);
    }

    return FALSE;
  }

  return PL_type_error("c_ptr", t);
}


		 /*******************************
		 *	  PROLOG BINDING	*
		 *******************************/

static int
get_type(term_t t, atom_t *tname, type_qualifier *tq)
{ atom_t qn;
  size_t arity;
  module_t m = NULL;

  if ( !PL_strip_module(t, &m, t) )		/* module just ignored for now */
    return FALSE;

  if ( PL_get_atom(t, tname) )
  { *tq = Q_PLAIN;
    return TRUE;
  } else if ( PL_get_name_arity(t, &qn, &arity) && arity == 1 )
  { term_t a = PL_new_term_ref();

    _PL_get_arg(1, t, a);
    if ( PL_get_atom_ex(a, tname) )
    { if ( qn == ATOM_struct )
	*tq = Q_STRUCT;
      else if ( qn == ATOM_union )
	*tq = Q_UNION;
      else if ( qn == ATOM_enum )
	*tq = Q_ENUM;
      else
	return PL_type_error("c_type", t);
    }

    return TRUE;
  } else
    return PL_type_error("c_type", t);

  return FALSE;
}


static int
unify_type(term_t t, atom_t tname, type_qualifier q)
{ if ( q == Q_PLAIN )
  { return PL_unify_atom(t, tname);
  } else
  { functor_t f;

    if      ( q == Q_STRUCT )  f = FUNCTOR_struct1;
    else if ( q == Q_UNION  )  f = FUNCTOR_union1;
    else /*if ( q == Q_ENUM )*/f = FUNCTOR_enum1;

    return PL_unify_term(t, PL_FUNCTOR, f, PL_ATOM, tname);
  }
}



static foreign_t
c_calloc(term_t ptr, term_t type, term_t esize, term_t count)
{ size_t esz;
  size_t cnt;
  atom_t ta;
  type_qualifier q;

  if ( get_type(type, &ta, &q) &&
       PL_get_size_ex(esize, &esz) &&
       PL_get_size_ex(count, &cnt) )
  { size_t bytes = esz*cnt;
    void *p = malloc(bytes);

    if ( p )
    { memset(p, 0, bytes);
      if ( unify_ptr(ptr, p, cnt, esz, ta, q, free) )
	return TRUE;
      free(p);
    } else
      PL_resource_error("memory");
  }

  return FALSE;
}




static foreign_t
c_realloc(term_t ptr, term_t size)
{ size_t sz;
  PL_blob_t *type;
  void *bp;

  if ( PL_get_size_ex(size, &sz) &&
       PL_get_blob(ptr, &bp, NULL, &type) &&
       type == &c_ptr_blob )
  { c_ptr *ref = bp;
    void *p = realloc(ref->ptr, sz);

    if ( p )
    { memset(p, 0, sz);
      if ( sz > ref->size )
	memset((char*)p+ref->size, 0, sz-ref->size);
      ref->size = sz;
      return TRUE;
    } else
      PL_resource_error("memory");
  }

  return FALSE;
}


static foreign_t
c_free(term_t ptr)
{ PL_blob_t *type;
  void *bp;

  if ( PL_get_blob(ptr, &bp, NULL, &type) &&
       type == &c_ptr_blob )
  { free_ptr(bp);
    return TRUE;
  }

  return PL_type_error("c_ptr", ptr);
}


static foreign_t
c_typeof(term_t ptr, term_t type)
{ c_ptr *ref;

  if ( (ref=get_ptr_ref_ex(ptr, NULL)) )
    return unify_type(type, ref->type, ref->qual);

  return FALSE;
}


#define VALID(ref, off, type) \
	(((off)+sizeof(type) <= ref->count*ref->size) ? TRUE : \
	PL_domain_error("offset", offset))

static foreign_t
c_load(term_t ptr, term_t offset, term_t type, term_t value)
{ PL_blob_t *btype;
  void *bp;
  size_t off;

  if ( PL_get_blob(ptr, &bp, NULL, &btype) &&
       btype == &c_ptr_blob &&
       PL_get_size_ex(offset, &off) )
  { atom_t ta;
    size_t tarity;
    c_ptr *ref = bp;
    void *vp = (void*)((char *)ref->ptr + off);

    if ( PL_get_atom(type, &ta) )
    { if ( ta == ATOM_char )
      { const char *p = vp;
	return VALID(ref, off, char) && PL_unify_integer(value, *p);
      } else if ( ta == ATOM_uchar )
      { const unsigned char *p = vp;
	return VALID(ref, off, char) && PL_unify_integer(value, *p);
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
	return VALID(ref, off, void*) &&
	       unify_ptr(value, *p, SZ_UNKNOWN, 1,
			 ATOM_void, Q_PLAIN, NULL);
      } else
	return PL_domain_error("c_type", type);
    } else if ( PL_get_name_arity(type, &ta, &tarity) )
    { term_t arg = PL_new_term_ref();

      if ( ta == ATOM_pointer && tarity == 1 )
      { atom_t atype;
	type_qualifier q;

	_PL_get_arg(1, type, arg);
	if ( get_type(arg, &atype, &q) )
	{ void **p = vp;
	  return VALID(ref, off, void*) &&
		 unify_ptr(value, *p, SZ_UNKNOWN, 1, atype, q, NULL);
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
  size_t sz;
  size_t cnt;
  atom_t ta;
  type_qualifier q;

  if ( (ref=get_ptr_ref_ex(ptr0, &ptra)) &&
       PL_get_size_ex(offset, &off) &&
       PL_get_size_ex(size, &sz) &&
       get_count_or_unknown(count, &cnt) &&
       get_type(type, &ta, &q) )
  { void *vp = (void*)((char *)ref->ptr + off);

    if ( unify_ptr(ptr, vp, cnt, sz, ta, q, NULL) )
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
    else if ( ta == ATOM_short )     sz = __alignof__(short);
    else if ( ta == ATOM_ushort )    sz = __alignof__(unsigned short);
    else if ( ta == ATOM_int )       sz = __alignof__(int);
    else if ( ta == ATOM_uint )      sz = __alignof__(unsigned int);
    else if ( ta == ATOM_long )      sz = __alignof__(long);
    else if ( ta == ATOM_ulong )     sz = __alignof__(unsigned long);
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
  } else if ( aenc == ATOM_text )
  { flags |= REP_MB;
  } else
  { if ( aenc == ATOM_wchar_t )
    { pl_wchar_t *ws;

      if ( PL_get_wchars(data, &len, &ws, flags) )
      { if ( unify_ptr(ptr, ws, (len+1), sizeof(pl_wchar_t),
		       ATOM_wchar_t, Q_PLAIN, PL_free) )
	  return TRUE;
	PL_free(ws);
      }
    } else
      return PL_domain_error("encoding", encoding);
  }

  if ( PL_get_nchars(data, &len, &s, flags) )
  { if ( unify_ptr(ptr, s, len+1, 1, ATOM_char, Q_PLAIN, PL_free) )
      return TRUE;
    PL_free(s);
  }

  return FALSE;
}


static foreign_t
c_load_string(term_t ptr, term_t data, term_t type, term_t encoding)
{ PL_blob_t *btype;
  void *bp;

  if ( PL_get_blob(ptr, &bp, NULL, &btype) &&
       btype == &c_ptr_blob )
  { c_ptr *ref = bp;
    atom_t aenc, atype;
    int flags = 0;

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
      return PL_unify_wchars(data, flags, (size_t)-1, ref->ptr);

    if ( aenc == ATOM_iso_latin_1 )
    { flags |= REP_ISO_LATIN_1;
    } else if ( aenc == ATOM_utf8 )
    { flags |= REP_UTF8;
    } else if ( aenc == ATOM_text )
    { flags |= REP_MB;
    } else
      return PL_domain_error("encoding", encoding);

    return PL_unify_chars(data, flags, (size_t)-1, ref->ptr);
  }

  return FALSE;
}


#define MKATOM(n) \
        ATOM_ ## n = PL_new_atom(#n)
#define MKFUNCTOR(n,a) \
        FUNCTOR_ ## n ## a = PL_new_functor(PL_new_atom(#n), a)

static install_t
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
  MKATOM(void);
  MKATOM(iso_latin_1);
  MKATOM(utf8);
  MKATOM(text);
  MKATOM(wchar_t);
  MKATOM(char);
  MKATOM(atom);
  MKATOM(string);
  MKATOM(codes);
  MKATOM(chars);
  MKATOM(struct);
  MKATOM(union);
  MKATOM(enum);

  MKFUNCTOR(struct, 1);
  MKFUNCTOR(union, 1);
  MKFUNCTOR(enum, 1);
  FUNCTOR_array2 = PL_new_functor(ATOM_nil, 2);

  PL_register_foreign("c_calloc",	4, c_calloc,	   0);
  PL_register_foreign("c_realloc",	2, c_realloc,	   0);
  PL_register_foreign("c_free",		1, c_free,	   0);
  PL_register_foreign("c_load",		4, c_load,	   0);
  PL_register_foreign("c_store",	4, c_store,	   0);
  PL_register_foreign("c_offset",	6, c_offset,	   0);
  PL_register_foreign("c_address",	2, c_address,	   0);
  PL_register_foreign("c_typeof",	2, c_typeof,	   0);
  PL_register_foreign("c_sizeof",	2, c_sizeof,	   0);
  PL_register_foreign("c_alignof",	2, c_alignof,	   0);
  PL_register_foreign("c_alloc_string",	3, c_alloc_string, 0);
  PL_register_foreign("c_load_string",	4, c_load_string,  0);
}
