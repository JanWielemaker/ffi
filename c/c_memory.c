/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
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

typedef struct c_ptr
{ void *ptr;				/* the pointer */
  atom_t type;				/* Its type */
  size_t size;				/* Size behind ptr */
  void *ctx;				/* Pointer context */
  void (*free)(void *ptr);		/* Its free function */
} c_ptr;


static int
free_ptr(c_ptr *ref)
{ void *p = ref->ptr;

  if ( __sync_bool_compare_and_swap(&ref->ptr, p, NULL) )
  { if ( ref->free )
      (*ref->free)(p);
    else
      free(p);

    return TRUE;
  }

  return FALSE;
}


static int
write_c_ptr(IOSTREAM *s, atom_t aref, int flags)
{ c_ptr *ref = PL_blob_data(aref, NULL, NULL);
  (void)flags;

  Sfprintf(s, "<c_ptr>(%s,%p)", PL_atom_chars(ref->type), ref->ptr);
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

  free_ptr(ref);
  free(ref);

  return TRUE;
}


static int
save_c_ptr(atom_t aref, IOSTREAM *fd)
{ c_ptr *ref = PL_blob_data(aref, NULL, NULL);
  (void)fd;

  return PL_warning("Cannot save reference to <c_ptr>(%s,%p)",
		    PL_atom_chars(ref->type), ref->ptr);
}


static atom_t
load_c_ptr(IOSTREAM *fd)
{ (void)fd;

  return PL_new_atom("<c_ptr>");
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
unify_ptr(term_t t, void *ptr, void *ctx, atom_t type)
{ c_ptr *ref = malloc(sizeof(*ref));

  if ( ref )
  { ref->ptr  = ptr;
    ref->type = type;
    ref->ctx  = ctx;
    ref->free = NULL;
  }

  return PL_unify_blob(t, ref, sizeof(*ref), &c_ptr_blob);
}


static int
get_ptr(term_t t, void *ptrp, void *ctxp, atom_t ptrtype)
{ PL_blob_t *type;
  void *bp;

  if ( PL_get_blob(t, &bp, NULL, &type) &&
       type == &c_ptr_blob )
  { c_ptr *ref = bp;
    void **ptrpp = ptrp;

    if ( ptrtype && ptrtype != ref->type )
      return PL_type_error(PL_atom_chars(ref->type), t);

    *ptrpp = ref->ptr;
    if ( ctxp )
    { void **ctxpp = ctxp;
      *ctxpp = ref->ctx;
    }

    return TRUE;
  }

  return PL_type_error("c_ptr", t);
}


		 /*******************************
		 *	  PROLOG BINDING	*
		 *******************************/

static foreign_t
c_alloc(term_t ptr, term_t type, term_t size)
{ size_t sz;
  atom_t ta;

  if ( PL_get_atom_ex(type, &ta) &&
       PL_get_size_ex(size, &sz) )
  { void *p = malloc(sz);

    if ( p )
    { memset(p, 0, sz);
      if ( unify_ptr(ptr, p, NULL, ta) )
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
{ PL_blob_t *btype;
  void *bp;

  if ( PL_get_blob(ptr, &bp, NULL, &btype) &&
       btype == &c_ptr_blob )
  { c_ptr *ref = bp;

    return PL_unify_atom(type, ref->type);
  }

  return PL_type_error("c_ptr", ptr);
}


static foreign_t
c_load(term_t ptr, term_t offset, term_t type, term_t value)
{ PL_blob_t *btype;
  void *bp;
  atom_t ta;
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
	return PL_unify_integer(value, *p);
      } else if ( ta == ATOM_uchar )
      { const unsigned char *p = vp;
	return PL_unify_integer(value, *p);
      } else if ( ta == ATOM_short )
      { const short *p = vp;
	return PL_unify_integer(value, *p);
      } else if ( ta == ATOM_ushort )
      { const unsigned short *p = vp;
	return PL_unify_integer(value, *p);
      } else if ( ta == ATOM_int )
      { const int *p = vp;
	return PL_unify_integer(value, *p);
      } else if ( ta == ATOM_uint )
      { const unsigned int *p = vp;
	return PL_unify_uint64(value, *p);
      } else if ( ta == ATOM_long )
      { const long *p = vp;
	return PL_unify_integer(value, *p);
      } else if ( ta == ATOM_ulong )
      { const unsigned long *p = vp;
	return PL_unify_uint64(value, *p);
      } else if ( ta == ATOM_longlong )
      { const long long *p = vp;
	int64_t v = (int64_t)*p;
	return PL_unify_integer(value, v);
      } else if ( ta == ATOM_ulonglong )
      { const unsigned long long *p = vp;
	uint64_t v = (uint64_t)*p;
	return PL_unify_uint64(value, v);
      } else if ( ta == ATOM_float )
      { const float *p = vp;
	return PL_unify_float(value, *p);
      } else if ( ta == ATOM_double )
      { const double *p = vp;
	return PL_unify_float(value, *p);
      } else if ( ta == ATOM_pointer )
      { void **p = vp;
	return unify_ptr(value, *p, NULL, ATOM_void);
      } else
	return PL_domain_error("c_type", type);
    } else if ( PL_get_name_arity(type, &ta, &tarity) )
    { term_t arg = PL_new_term_ref();

      if ( ta == ATOM_pointer && tarity == 1 )
      { atom_t atype;

	_PL_get_arg(1, type, arg);
	if ( PL_get_atom_ex(arg, &atype) )
	{ void **p = vp;
	  return unify_ptr(value, *p, NULL, atype);
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
i_ptr(term_t value, void **vp)
{ void *p;

  if ( get_ptr(value, &p, NULL, 0) )
  { *vp = p;
    return TRUE;
  }

  return FALSE;
}


static foreign_t
c_store(term_t ptr, term_t offset, term_t type, term_t value)
{ PL_blob_t *btype;
  void *bp;
  atom_t ta;
  size_t off;

  if ( PL_get_blob(ptr, &bp, NULL, &btype) &&
       btype == &c_ptr_blob &&
       PL_get_size_ex(offset, &off) )
  { atom_t ta;
    size_t tarity;
    c_ptr *ref = bp;
    void *vp = (void*)((char *)ref->ptr + off);

    if ( PL_get_atom(type, &ta) )
    {      if ( ta == ATOM_char )      return PL_cvt_i_char(value, vp);
      else if ( ta == ATOM_uchar )     return PL_cvt_i_uchar(value, vp);
      else if ( ta == ATOM_short )     return PL_cvt_i_short(value, vp);
      else if ( ta == ATOM_ushort )    return PL_cvt_i_ushort(value, vp);
      else if ( ta == ATOM_int )       return PL_cvt_i_int(value, vp);
      else if ( ta == ATOM_uint )      return PL_cvt_i_uint(value, vp);
      else if ( ta == ATOM_long )      return PL_cvt_i_long(value, vp);
      else if ( ta == ATOM_ulong )     return PL_cvt_i_ulong(value, vp);
      else if ( ta == ATOM_longlong )  return PL_cvt_i_int64(value, vp);
      else if ( ta == ATOM_ulonglong ) return PL_cvt_i_uint64(value, vp);
      else if ( ta == ATOM_float )     return PL_cvt_i_single(value, vp);
      else if ( ta == ATOM_double )    return PL_cvt_i_float(value, vp);
      else if ( ta == ATOM_pointer )   return i_ptr(value, vp);
      else return PL_domain_error("c_type", type);
    }
  }

  return FALSE;
}


static foreign_t
c_sizeof(term_t type, term_t bytes)
{ atom_t ta;
  int sz;

  if ( PL_get_atom_ex(type, &ta) )
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
    else return PL_domain_error("c_type", type);

    return PL_unify_integer(bytes, sz);
  }

  return FALSE;
}


static foreign_t
c_alignof(term_t type, term_t bytes)
{ atom_t ta;
  int sz;

  if ( PL_get_atom_ex(type, &ta) )
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
    else return PL_domain_error("c_type", type);

    return PL_unify_integer(bytes, sz);
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

  PL_register_foreign("c_alloc",   3, c_alloc,   0);
  PL_register_foreign("c_realloc", 2, c_realloc, 0);
  PL_register_foreign("c_free",    1, c_free,    0);
  PL_register_foreign("c_load",    4, c_load,    0);
  PL_register_foreign("c_store",   4, c_store,   0);
  PL_register_foreign("c_typeof",  2, c_typeof,  0);
  PL_register_foreign("c_sizeof",  2, c_sizeof,  0);
  PL_register_foreign("c_alignof", 2, c_alignof, 0);
}
