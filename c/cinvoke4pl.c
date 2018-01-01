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
#include <cinvoke.h>
#include <string.h>
#include <assert.h>

static unsigned int debug = 0;

#define DEBUG(l, g) if ( (l) <= debug ) do { g; } while(0)

static atom_t ATOM_ci_context;
static atom_t ATOM_ci_library;
static atom_t ATOM_ci_entrypoint;
static atom_t ATOM_ci_function;

static atom_t ATOM_cdecl;
static atom_t ATOM_stdcall;
static atom_t ATOM_fastcall;

static atom_t ATOM_void;			/* void */

#include "c_memory.c"


		 /*******************************
		 *	    CONVERSIONS		*
		 *******************************/

static int
get_cc(term_t cc, cinv_callconv_t *v)
{ atom_t a;

  if ( PL_get_atom_ex(cc, &a) )
  { if      ( a == ATOM_cdecl )    *v = CINV_CC_CDECL;
    else if ( a == ATOM_stdcall )  *v = CINV_CC_STDCALL;
    else if ( a == ATOM_fastcall ) *v = CINV_CC_STDCALL;
    else return PL_domain_error("ic_calling_convention", cc);

    return TRUE;
  }

  return FALSE;
}


/*
static int
get_type(term_t type, cinv_type_t *v)
{ atom_t a;

  if ( PL_get_atom_ex(type, &a) )
  { if      ( a == ATOM_char )     *v = CINV_T_CHAR;
    else if ( a == ATOM_short )    *v = CINV_T_SHORT;
    else if ( a == ATOM_int )      *v = CINV_T_INT;
    else if ( a == ATOM_long )     *v = CINV_T_LONG;
    else if ( a == ATOM_longlong ) *v = CINV_T_EXTRALONG;
    else if ( a == ATOM_float )    *v = CINV_T_FLOAT;
    else if ( a == ATOM_double )   *v = CINV_T_DOUBLE;
    else if ( a == ATOM_pointer )  *v = CINV_T_PTR;
    else return PL_domain_error("ic_type", type);

    return TRUE;
  }

  return FALSE;
}
*/


		 /*******************************
		 *	      ERRORS		*
		 *******************************/

static int
ci_error(CInvContext *cictx)
{ Sdprintf("Error!\n");
  return FALSE;
}

static int
ci_status(cinv_status_t status, CInvContext *cictx)
{ if ( status == CINV_SUCCESS )
  { return TRUE;
  } else
  { return ci_error(cictx);
  }
}


		 /*******************************
		 *	   CONTEXT TYPES	*
		 *******************************/

typedef struct ctx_context
{ CInvContext  *cictx;
  atom_t	symbol;
} ctx_context;

typedef struct ctx_library
{ ctx_context  *cictx;
  CInvLibrary  *lib;
} ctx_library;

typedef struct ctx_entrypoint
{ ctx_context  *cictx;
  void         *func;
} ctx_entrypoint;

typedef struct ctx_prototype
{ ctx_context  *cictx;
  CInvFunction *func;
  void         *entrypoint;
  const char   *rformat;
  const char   *pformat;
} ctx_prototype;


		 /*******************************
		 *	 LINK DEPENDENCIES	*
		 *******************************/

static int
unify_part_ptr(term_t t, ctx_context *ctx,
	       void *ptr, size_t size, atom_t type,
	       freefunc free)
{ if ( unify_ptr(t, ptr, 1, size, type, Q_STRUCT, free) )
  { PL_register_atom(ctx->symbol);
    return TRUE;
  }

  return FALSE;
}



		 /*******************************
		 *		API		*
		 *******************************/

static void
ci_context_free(void *ptr)
{ ctx_context *ctx = ptr;

  cinv_context_delete(ctx->cictx);
  free(ctx);
}


static foreign_t
ci_context_create(term_t context)
{ CInvContext *cictx;

  if ( (cictx=cinv_context_create()) )
  { ctx_context *ctx = malloc(sizeof(*ctx));

    if ( ctx )
    { ctx->cictx = cictx;

      if ( unify_ptr(context, ctx, 1, sizeof(*ctx),
		     ATOM_ci_context, Q_STRUCT, ci_context_free) )
	return PL_get_atom(context, &ctx->symbol);

      free(ctx);
    }
    cinv_context_delete(cictx);
    if ( !PL_exception(0) )
      PL_resource_error("memory");
  }

  return FALSE;
}


static void
ci_library_free(void *ptr)
{ ctx_library *libh = ptr;

  if ( libh->lib )
    cinv_library_delete(libh->cictx->cictx, libh->lib);
  PL_unregister_atom(libh->cictx->symbol);

  free(ptr);
}


static foreign_t
ci_library_create(term_t context, term_t path, term_t lib)
{ char *name;
  ctx_context *ctx;

  if ( get_ptr(context, &ctx, ATOM_ci_context) &&
       PL_get_file_name(path, &name,
			PL_FILE_OSPATH|PL_FILE_SEARCH|PL_FILE_READ) )
  { CInvLibrary *h;

    DEBUG(1, Sdprintf("Opening %s\n", name));

    if ( (h=cinv_library_create(ctx->cictx, name)) )
    { ctx_library *libh = malloc(sizeof(*libh));

      if ( libh )
      { libh->cictx = ctx;
	libh->lib   = h;

	if ( unify_part_ptr(lib, ctx,
			    libh, sizeof(*libh), ATOM_ci_library,
			    ci_library_free) )
	  return TRUE;

	free(libh);
      }

      cinv_library_delete(ctx->cictx, h);
      if ( !PL_exception(0) )
	PL_resource_error("memory");
      return FALSE;
    }

    return ci_error(ctx->cictx);
  }

  return FALSE;
}


static foreign_t
ci_free_library(term_t lib)
{ ctx_library *libh;

  if ( get_ptr(lib, &libh, ATOM_ci_library) )
  { cinv_status_t rc;
    CInvLibrary *h = libh->lib;

    if ( h &&__sync_bool_compare_and_swap(&libh->lib, h, NULL) )
    { rc = cinv_library_delete(libh->cictx->cictx, h);
      return ci_status(rc, libh->cictx->cictx);
    }

    return TRUE;
  }

  return FALSE;
}


static void
ci_entrypoint_free(void *ptr)
{ ctx_entrypoint *ep = ptr;

  PL_unregister_atom(ep->cictx->symbol);
  free(ptr);
}


static foreign_t
ci_library_load_entrypoint(term_t lib, term_t name, term_t func)
{ ctx_library *libh;
  char *fname;

  if ( get_ptr(lib, &libh, ATOM_ci_library) &&
       PL_get_chars(name, &fname, CVT_ATOM|CVT_EXCEPTION) )
  { void *f;

    DEBUG(1, Sdprintf("Find %s in %p\n", fname, libh));

    if ( (f=cinv_library_load_entrypoint(libh->cictx->cictx, libh->lib, fname)) )
    { ctx_entrypoint *ep = malloc(sizeof(*ep));

      if ( ep )
      { ep->cictx = libh->cictx;
	ep->func  = f;

	if ( unify_part_ptr(func, libh->cictx,
			    ep, sizeof(*ep), ATOM_ci_entrypoint,
			    ci_entrypoint_free) )
	  return TRUE;

	free(ep);
	return FALSE;
      }

      return PL_resource_error("memory");
    }
  }

  return FALSE;
}


static int
ci_signature(const char *s, char *buf)
{ char *o = buf;
  int size = 0;					/* -2..2 */

  for(; *s; s++)
  { switch(*s)
    { case 'u':					/* modifiers */
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
	{ case -2: *o++ = 'c'; break;
          case -1: *o++ = 's'; break;
	  case  0: *o++ = 'i'; break;
	  case  1: *o++ = 'l'; break;
	  case  2: *o++ = 'e'; break;
	  default: return PL_syntax_error("invalid signature", NULL);
	}
        break;
      case 'f':
	switch(size)
	{ case  0: *o++ = 'f'; break;
	  case  1: *o++ = 'd'; break;
	  default: return PL_syntax_error("invalid signature", NULL);
	}
        break;
      case 'p':
	*o++ = 'p';
        break;
      default:
	return PL_syntax_error("invalid signature", NULL);
    }

    size = 0;
  }

  *o = '\0';
  return TRUE;
}


static void
ci_function_free(void *ptr)
{ ctx_prototype *p = ptr;

  cinv_function_delete(p->cictx->cictx, p->func);

  free(p);
}


static foreign_t
ci_function_create(term_t entry, term_t cc, term_t ret, term_t parms, term_t func)
{ ctx_entrypoint *ep;
  cinv_callconv_t ccv;
  char *rformat;
  char *pformat;
  char ci_rformat[16];
  char ci_pformat[100];

  if ( get_ptr(entry, &ep, ATOM_ci_entrypoint) &&
       get_cc(cc, &ccv) &&
       PL_get_chars(ret, &rformat, CVT_ATOM|CVT_STRING|CVT_EXCEPTION) &&
       PL_get_chars(parms, &pformat, CVT_ATOM|CVT_STRING|CVT_EXCEPTION) &&
       ci_signature(rformat, ci_rformat) &&
       ci_signature(pformat, ci_pformat) )
  { CInvFunction *f;

    DEBUG(1, Sdprintf("Created function with (%s)%s\n", ci_pformat, ci_rformat));

    if ( (f=cinv_function_create(ep->cictx->cictx, ccv, ci_rformat, ci_pformat)) )
    { ctx_prototype *p = malloc(sizeof(*p));

      if ( p )
      { memset(p, 0, sizeof(*p));
	p->cictx      = ep->cictx;
	p->entrypoint = ep->func;
	p->func	      = f;
	p->rformat    = strdup(rformat);
	p->pformat    = strdup(pformat);

	if ( unify_part_ptr(func, ep->cictx,
			    p, sizeof(*p), ATOM_ci_function,
			    ci_function_free) )
	  return TRUE;

	free(p);
      }

      cinv_function_delete(ep->cictx->cictx, f);
      if ( !PL_exception(0) )
	PL_resource_error("memory");

      return FALSE;
    }
    return ci_error(ep->cictx->cictx);
  }

  return FALSE;
}


#define MAXARGC 10

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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
What do we want to know about an argument?

  - Scalars
    - Float/Int
    - Size
    - Unsigned (int)
  - Pointers
    - Input, output (both?)
    - Strings
      - Encoding
      - If output, free?
    - Structs

Encoding:

  - Primary type: i, f, p (integer, float, pointer)
  - Integer
    - size: hh (char), h (short), l (long), ll (long long)
    - signed: u (unsigned) s (signed)
  - Float size: l (double)
  - Pointer:
    - +,-: input/output
    - s (string), w (wide string)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


static foreign_t
ci_function_invoke(term_t prototype, term_t goal)
{ ctx_prototype *ctx;

  if ( get_ptr(prototype, &ctx, ATOM_ci_function) )
  { void *argv[MAXARGC];
    argstore as[MAXARGC];
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

    cinv_function_invoke(ctx->cictx->cictx, ctx->func, ctx->entrypoint,
			 &rv.p, argv);

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
	    return unify_ptr(arg, rv.p, SZ_UNKNOWN, 1,
			     ATOM_void, Q_PLAIN, NULL);
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
ci_debug(term_t level)
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
{ MKATOM(ci_context);
  MKATOM(ci_library);
  MKATOM(ci_entrypoint);
  MKATOM(ci_function);

  MKATOM(cdecl);
  MKATOM(stdcall);
  MKATOM(fastcall);

  MKATOM(void);

  install_c_memory();

  PL_register_foreign("ci_context_create",  1, ci_context_create,  0);
  PL_register_foreign("ci_library_create",  3, ci_library_create,  0);
  PL_register_foreign("ci_free_library",    1, ci_free_library,    0);
  PL_register_foreign("ci_library_load_entrypoint",
					    3, ci_library_load_entrypoint, 0);
  PL_register_foreign("ci_function_create", 5, ci_function_create, 0);
  PL_register_foreign("ci_function_invoke", 2, ci_function_invoke, 0);

  PL_register_foreign("ci_debug",           1, ci_debug,           0);
  PL_register_foreign("c_errno",            1, c_errno,            0);
}
