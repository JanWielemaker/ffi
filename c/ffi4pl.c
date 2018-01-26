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
#include "../config.h"
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#ifdef HAVE_FFI_FFI_H
#include <ffi/ffi.h>
#else
#include <ffi.h>
#endif
#include <dlfcn.h>
#include <errno.h>
#include <string.h>
#include <assert.h>

#define MAX_ARGC 100

static unsigned int debug = 0;

#define DEBUG(l, g) if ( (l) <= debug ) do { g; } while(0)

static atom_t ATOM_c_library;
static atom_t ATOM_c_symbol;
static atom_t ATOM_c_function;

static atom_t ATOM_default;
static atom_t ATOM_cdecl;
static atom_t ATOM_stdcall;
static atom_t ATOM_fastcall;

static atom_t ATOM_void;			/* void */

static functor_t FUNCTOR_pointer2;
static functor_t FUNCTOR_pointer3;

#include "cmemory.c"

		 /*******************************
		 *	      TYPES		*
		 *******************************/

typedef struct ret_spec
{ atom_t	 type;
  type_qualifier qual;
  size_t	 size;			/* Element size */
  void          *free;			/* Free function */
} ret_spec;

typedef struct ctx_library
{ char	       *name;			/* name of the library */
  void	       *lib;			/* handle */
} ctx_library;

typedef struct ctx_symbol
{ void         *func;
} ctx_symbol;

typedef struct ctx_prototype
{ ffi_cif	cif;			/* libffi cif */
  void         *func;
  int		argc;
  const char   *rformat;
  const char   *pformat;
  ffi_type    **atypes;
  ret_spec	ret;
} ctx_prototype;



		 /*******************************
		 *	    CONVERSIONS		*
		 *******************************/

static int
get_abi(term_t cc, ffi_abi *v)
{ atom_t a;

  if ( PL_get_atom_ex(cc, &a) )
  { if      ( a == ATOM_default )  *v = FFI_DEFAULT_ABI;
    else
    { PL_domain_error("ffi_abi", cc);
      return FALSE;
    }

    return TRUE;
  }

  return FALSE;
}


int
get_return(term_t t, ret_spec *rspec, char **format)
{ int nofree;

  memset(rspec, 0, sizeof(*rspec));

  if ( (nofree = PL_is_functor(t, FUNCTOR_pointer2)) ||
       PL_is_functor(t, FUNCTOR_pointer3) )
  { term_t a = PL_new_term_ref();
    static char *p = "p";

    _PL_get_arg(1, t, a);
    if ( !get_type(a, &rspec->type, &rspec->qual) )
      return FALSE;
    _PL_get_arg(2, t, a);
    if ( !PL_get_size_ex(a, &rspec->size) )
      return FALSE;

    if ( !nofree )
    { ctx_symbol *ep;

      _PL_get_arg(3, t, a);
      if ( get_ptr(a, &ep, ATOM_c_symbol) )
	rspec->free = ep->func;
      else
	return FALSE;
    }

    *format = p;
  } else
  { if ( !PL_get_chars(t, format, CVT_ATOM|CVT_STRING|CVT_EXCEPTION) )
      return FALSE;
    rspec->type = ATOM_void;
    rspec->qual = Q_PLAIN;
    rspec->size = SZ_UNKNOWN;
  }

  return TRUE;
}



		 /*******************************
		 *	      ERRORS		*
		 *******************************/

static int
dl_error(const char *op, const char *obj)
{ Sdprintf("Error: *s: %s: %s\n", op, obj, dlerror());

  return FALSE;
}

static int
ffi_error(ffi_status rc)
{ switch(rc)
  { case FFI_OK:
      return TRUE;
    case FFI_BAD_TYPEDEF:
      Sdprintf("Bad typedef\n");
      return FALSE;
    case FFI_BAD_ABI:
      Sdprintf("Bad ABI\n");
      return FALSE;
    default:
      Sdprintf("FFI: unknown error %d\n", (int)rc);
      return FALSE;
  }
}

		 /*******************************
		 *	 LINK DEPENDENCIES	*
		 *******************************/

static int
unify_part_ptr(term_t t,
	       void *ptr, size_t size, atom_t type,
	       freefunc free)
{ return unify_ptr(t, ptr, 1, size, type, Q_STRUCT, free);
}



		 /*******************************
		 *		API		*
		 *******************************/

static void
ffi_library_free(void *ptr)
{ ctx_library *libh = ptr;

  if ( libh->lib )
    dlclose(libh->lib);
  if ( libh->name )
    free(libh->name);

  free(ptr);
}


static foreign_t
ffi_library_create(term_t path, term_t lib, term_t options)
{ char *name;

  if ( PL_get_file_name(path, &name,
			PL_FILE_OSPATH|PL_FILE_SEARCH|PL_FILE_READ) )
  { int flags = RTLD_LAZY;
    void *h;

    DEBUG(1, Sdprintf("Opening %s\n", name));

    if ( (h=dlopen(name, flags)) )
    { ctx_library *libh = malloc(sizeof(*libh));

      if ( libh )
      { libh->lib  = h;
	libh->name = strdup(name);

	if ( unify_part_ptr(lib, libh, sizeof(*libh), ATOM_c_library,
			    ffi_library_free) )
	  return TRUE;

	free(libh);
      }

      dlclose(h);
      if ( !PL_exception(0) )
	PL_resource_error("memory");
      return FALSE;
    }

    return dl_error("dlopen", name);
  }

  return FALSE;
}


static foreign_t
pl_ffi_library_free(term_t lib)
{ ctx_library *libh;

  if ( get_ptr(lib, &libh, ATOM_c_library) )
  { void *h = libh->lib;

    if ( h &&__sync_bool_compare_and_swap(&libh->lib, h, NULL) )
    { if ( dlclose(h) )
	return dl_error("dlclose", libh->name);
    }

    return TRUE;
  }

  return FALSE;
}


static void
ffi_symbol_free(void *ptr)
{ ctx_symbol *ep = ptr;

  free(ep);
}


static foreign_t
ffi_lookup_symbol(term_t lib, term_t name, term_t func)
{ ctx_library *libh;
  char *fname;

  if ( get_ptr(lib, &libh, ATOM_c_library) &&
       PL_get_chars(name, &fname, CVT_ATOM|CVT_EXCEPTION) )
  { void *f;

    DEBUG(1, Sdprintf("Find %s in %p ...\n", fname, libh));

    if ( (f=dlsym(libh->lib, fname)) )
    { ctx_symbol *ep = malloc(sizeof(*ep));

      if ( ep )
      { ep->func  = f;

	DEBUG(1, Sdprintf("Found %s at %p ...\n", fname, f));

	if ( unify_part_ptr(func,
			    ep, sizeof(*ep), ATOM_c_symbol,
			    ffi_symbol_free) )
	  return TRUE;

	free(ep);
	return FALSE;
      }

      return PL_resource_error("memory");
    } else
      return FALSE;
  }

  return FALSE;
}


static int
ci_signature(const char *s, ffi_type **types)
{ ffi_type **o = types;
  int size = 0;					/* -2..2 */
  int u = FALSE;

  for(; *s; s++)
  { switch(*s)
    { case 'u':					/* modifiers */
	u = TRUE;
        /*FALLTHROUGH*/
      case '+':
      case '-':
	continue;
      case 'h':
	size--;
        continue;
      case 'l':
	size++;
        continue;

      case 'i':
	switch(size)
	{ case -2: *o++ = u ? &ffi_type_uchar  : &ffi_type_schar;  break;
          case -1: *o++ = u ? &ffi_type_ushort : &ffi_type_sshort; break;
	  case  0: *o++ = u ? &ffi_type_uint   : &ffi_type_sint;   break;
	  case  1: *o++ = u ? &ffi_type_ulong  : &ffi_type_slong;  break;
	  case  2: *o++ = u ? &ffi_type_uint64 : &ffi_type_sint64; break;
	  default:
	    PL_syntax_error("invalid signature", NULL);
	    return -1;
	}
        break;
      case 'f':
	switch(size)
	{ case  0: *o++ = &ffi_type_float; break;
	  case  1: *o++ = &ffi_type_double; break;
	  case  2: *o++ = &ffi_type_longdouble; break;
	  default:
	    PL_syntax_error("invalid signature", NULL);
	    return -1;
	}
        break;
      case 'p':
	switch(size)
	{ case 0: *o++ = &ffi_type_pointer; break;
	  default:
	    PL_syntax_error("invalid signature", NULL);
	    return -1;
	}
        break;
      default:
	PL_syntax_error("invalid signature", NULL);
        return -1;
    }

    size = 0;
    u = FALSE;
  }

  return o-types;
}


static int
ret_signature(const char *rformat, ffi_type **r_type)
{ if ( rformat[0] )
    return ci_signature(rformat, r_type);

  *r_type = &ffi_type_void;
  return TRUE;
}


static void
ci_function_free(void *ptr)
{ ctx_prototype *p = ptr;

  if ( p->atypes )
    free(p->atypes);

  free(p);
}


/** 'ffi_prototype_create'(+Function, +ABI, +Return, +Params, -Prototype)

@arg Return is either text, pointer(+Type) or pointer(+Type, +FreeFunc)
*/

static foreign_t
ffi_prototype_create(term_t entry, term_t cc,
		     term_t ret, term_t parms,
		     term_t func)
{ ctx_symbol *ep;
  ffi_abi abi;
  char *rformat;
  char *pformat;
  ffi_type *r_type;
  ffi_type *a_types[MAX_ARGC];
  int argc;
  ret_spec rspec;

  if ( get_ptr(entry, &ep, ATOM_c_symbol) &&
       get_abi(cc, &abi) &&
       get_return(ret, &rspec, &rformat) &&
       PL_get_chars(parms, &pformat, CVT_ATOM|CVT_STRING|CVT_EXCEPTION) &&
       ret_signature(rformat, &r_type) &&
       (argc=ci_signature(pformat, a_types)) >= 0 )
  { ctx_prototype *p = malloc(sizeof(*p));
    ffi_type    **at = malloc(argc*sizeof(*at));

    DEBUG(1, Sdprintf("Prototype args ok (argc=%d)\n", argc));

    if ( p && at )
    { ffi_status rc;

      memset(p, 0, sizeof(*p));
      memcpy(at, a_types, argc*sizeof(*at));

      rc = ffi_prep_cif(&p->cif, abi, argc, r_type, at);
      if ( rc != FFI_OK )
      { free(p);
	return ffi_error(rc);
      }

      p->func	 = ep->func;
      p->argc	 = argc;
      p->rformat = strdup(rformat);
      p->pformat = strdup(pformat);
      p->atypes  = at;
      p->ret     = rspec;

      DEBUG(1, Sdprintf("Created prototype\n"));

      if ( unify_part_ptr(func,
			  p, sizeof(*p), ATOM_c_function,
			  ci_function_free) )
	return TRUE;
    }

    if (  p ) free(p);
    if ( at ) free(at);

    if ( !PL_exception(0) )
      PL_resource_error("memory");

    return FALSE;
  }

  return FALSE;
}

typedef union argstore
{ char c;
  unsigned char uc;
  short s;
  unsigned short us;
  int i;
  unsigned int ui;
  long l;
  unsigned long ul;
  int64_t ll;
  uint64_t ull;
  float f;
  double d;
  void *p;
} argstore;


static foreign_t
pl_ffi_call(term_t prototype, term_t goal)
{ ctx_prototype *ctx;

  if ( get_ptr(prototype, &ctx, ATOM_c_function) )
  { void *argv[ctx->argc];
    argstore as[ctx->argc];
    argstore rv;
    int argc = 0;
    term_t arg = PL_new_term_ref();
    const char *pfmt;
    int unsig = FALSE;
    int size = 0;				/* -2..2 */
    int io = 0;					/* -1: output, +1: input */

    for(pfmt = ctx->pformat; *pfmt; pfmt++)
    { switch(*pfmt)
      { case 'u':				/* modifiers */
	  unsig = TRUE;
	  continue;
	case 'h':
	  size--;
	  continue;
	case 'l':
	  size++;
	  continue;
	case '+':
	  io = 1;
	  continue;
	case '-':
	  io = -1;
	  continue;
      }

      (void)io;					/* what to do? */

      if ( !PL_get_arg(argc+1, goal, arg) )
      { return ( PL_put_integer(arg, argc+1) &&
		 PL_existence_error("d_arg", arg) );
      }

      switch(*pfmt)
      { case 'i':				/* scalars */
	  switch(size)
	  { case -2:
	      if ( unsig )
	      { if ( !PL_cvt_i_uchar(arg, &as[argc].uc) )
		  return FALSE;
	      } else
	      { if ( !PL_cvt_i_char(arg, &as[argc].c) )
		  return FALSE;
	      }
	      argv[argc] = &as[argc].c;
	      break;
	    case -1:
	      if ( unsig )
	      { if ( !PL_cvt_i_ushort(arg, &as[argc].us) )
		  return FALSE;
	      } else
	      { if ( !PL_cvt_i_short(arg, &as[argc].s) )
		  return FALSE;
	      }
	      argv[argc] = &as[argc].s;
	      break;
	    case 0:
	      if ( unsig )
	      { if ( !PL_cvt_i_uint(arg, &as[argc].ui) )
		  return FALSE;
	      } else
	      { if ( !PL_cvt_i_int(arg, &as[argc].i) )
		  return FALSE;
	      }
	      argv[argc] = &as[argc].i;
	      break;
	    case 1:
	      if ( unsig )
	      { if ( !PL_cvt_i_ulong(arg, &as[argc].ul) )
		  return FALSE;
	      } else
	      { if ( !PL_cvt_i_long(arg, &as[argc].l) )
		  return FALSE;
	      }
	      argv[argc] = &as[argc].l;
	      break;
	    case 2:
	      if ( unsig )
	      { if ( !PL_cvt_i_uint64(arg, &as[argc].ull) )
		  return FALSE;
	      } else
	      { if ( !PL_cvt_i_int64(arg, &as[argc].ll) )
		  return FALSE;
	      }
	      argv[argc] = &as[argc].ll;
	      break;
	    default:
	      assert(0);
	  }
	  break;
	case 'f':
	  switch(size)
	  { case 0:
	      if ( !PL_cvt_i_single(arg, &as[argc].f) )
		return FALSE;
	      argv[argc] = &as[argc].f;
	      break;
	    case 1:
	      if ( !PL_cvt_i_float(arg, &as[argc].d) )
		return FALSE;
	      argv[argc] = &as[argc].d;
	      break;
	    default:
	      assert(0);
	  }
	  break;
	case 'p':				/* pointers */
	  if ( !get_ptr(arg, &as[argc].p, 0) )
	    return FALSE;
	  DEBUG(2, Sdprintf("Got ptr %p\n", as[argc].p));
	  argv[argc] = &as[argc].p;
	  break;
	default:
	  assert(0);
      }

      argc++;
      unsig = FALSE;
      size = 0;
      io = 0;
    }

    if ( ctx->rformat && ctx->rformat[0] )
    { if ( !PL_get_arg(argc+1, goal, arg) )
      { return ( PL_put_integer(arg, argc+1) &&
		 PL_existence_error("d_arg", arg) );
      }
    }

    ffi_call(&ctx->cif, ctx->func, &rv.p, argv);

    if ( ctx->rformat && ctx->rformat[0] )
    { unsig = FALSE;
      size  = 0;				/* -2..2 */
      io    = 0;				/* -1: output, +1: input */

      for(pfmt=ctx->rformat; *pfmt; pfmt++)
      { switch(*pfmt)
	{ case 'u':				/* modifiers */
	    unsig = TRUE;
	    continue;
	  case 'h':
	    size--;
	    continue;
	  case 'l':
	    size++;
	    continue;
	  case '+':
	    io = 1;
	    continue;
	  case '-':
	    io = -1;
	    continue;

	  case 'i':
	    switch(size)
	    { case -2:
		return unsig ? PL_unify_uint64(arg, rv.uc)
	                     : PL_unify_int64(arg, rv.c);
	      case -1:
		return unsig ? PL_unify_uint64(arg, rv.us)
	                     : PL_unify_int64(arg, rv.s);
	      case  0:
		return unsig ? PL_unify_uint64(arg, rv.ui)
	                     : PL_unify_int64(arg, rv.i);
	      case  1:
		return unsig ? PL_unify_uint64(arg, rv.ul)
	                     : PL_unify_int64(arg, rv.l);
	      case  2:
		return unsig ? PL_unify_uint64(arg, rv.ull)
	                     : PL_unify_int64(arg, rv.ll);
	    }
	  case 'f':
	    switch(size)
	    { case 0:
		return PL_cvt_o_float(rv.f, arg);
	      case 1:
		return PL_cvt_o_float(rv.d, arg);
	    }
	  case 'p':
	    return unify_ptr(arg, rv.p, 1, ctx->ret.size,
			     ctx->ret.type, ctx->ret.qual, ctx->ret.free);
	}
      }
    } else
    { return TRUE;			/* void function */
    }
  }

  return FALSE;
}


		 /*******************************
		 *	       MISC		*
		 *******************************/

static foreign_t
ffi_debug(term_t level)
{ unsigned int i;

  if ( PL_cvt_i_uint(level, &i) )
  { debug = i;
    return TRUE;
  }

  return FALSE;
}


static foreign_t
c_errno(term_t t)
{ return PL_unify_integer(t, errno);
}

		 /*******************************
		 *	     REGISTER		*
		 *******************************/

#define MKATOM(n) \
        ATOM_ ## n = PL_new_atom(#n)
#define MKFUNCTOR(n,a) \
        FUNCTOR_ ## n ## a = PL_new_functor(PL_new_atom(#n), a)

install_t
install(void)
{ MKATOM(c_library);
  MKATOM(c_symbol);
  MKATOM(c_function);

  MKATOM(default);
  MKATOM(cdecl);
  MKATOM(stdcall);
  MKATOM(fastcall);

  MKATOM(void);

  MKFUNCTOR(pointer, 2);
  MKFUNCTOR(pointer, 3);

  install_c_memory();

  PL_register_foreign("ffi_library_create",   2, ffi_library_create,   0);
  PL_register_foreign("ffi_library_free",     1, pl_ffi_library_free,  0);
  PL_register_foreign("ffi_lookup_symbol",    3, ffi_lookup_symbol,    0);
  PL_register_foreign("ffi_prototype_create", 5, ffi_prototype_create, 0);
  PL_register_foreign("ffi_call",	      2, pl_ffi_call,	       0);

  PL_register_foreign("ffi_debug",	      1, ffi_debug,	       0);
  PL_register_foreign("c_errno",	      1, c_errno,	       0);
}
