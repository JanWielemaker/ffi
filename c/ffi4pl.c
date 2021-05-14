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
#ifdef __WINDOWS__
#else
#include "../config.h"
#endif
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#ifdef HAVE_FFI_FFI_H
#include <ffi/ffi.h>
#else
#include <ffi.h>
#endif
#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#else					/* use emulation from SWI-Prolog */
#define dlopen PL_dlopen
#define dlclose PL_dlclose
#define dlsym PL_dlsym
#define dlerror PL_dlerror
#endif /*HAVE_DLFCN_H*/
#include <errno.h>
#include <string.h>
#include <assert.h>

#define MAX_OUTPUT_ARGS 16

static unsigned int debug = 0;

#define DEBUG(l, g) if ( (l) <= debug ) do { g; } while(0)

static atom_t ATOM_c_library;
static atom_t ATOM_c_symbol;
static atom_t ATOM_c_function;
static atom_t ATOM_c_closure;

static atom_t ATOM_default;
static atom_t ATOM_cdecl;
static atom_t ATOM_stdcall;
static atom_t ATOM_fastcall;

static atom_t ATOM_lazy;
static atom_t ATOM_now;
static atom_t ATOM_global;
static atom_t ATOM_local;
static atom_t ATOM_nodelete;
static atom_t ATOM_noload;
static atom_t ATOM_deepbind;

static atom_t ATOM_void;			/* void */

static functor_t FUNCTOR_minus1;

static int	get_closure(term_t t, void **func);

#include "cmemory.c"

		 /*******************************
		 *	      TYPES		*
		 *******************************/

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
  ffi_type    **ffi_type;
  ffi_type     *ffi_ret;
  type_spec    *arg_type;
  type_spec	ret_type;
} ctx_prototype;

typedef struct ctx_closure
{ ffi_cif	cif;			/* libffi spec */
  ffi_type    **ffi_type;		/* libffi type spec */
  ffi_closure  *closure;		/* libffis notion of the closure */
  void	       *func;			/* created function pointer */
  predicate_t	predicate;		/* predicate to call */
  size_t	argc;			/* argument count */
  type_spec	ret_type;		/* return type */
  type_spec    *arg_type;		/* argument types */
} ctx_closure;


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


static int
get_free_func(term_t t, void **func)
{ ctx_symbol *ep;
  type_spec tspec = {CT_STRUCT, 0, 0, ATOM_c_symbol};

  if ( get_ptr(t, &ep, &tspec) )
  { *func = ep->func;
    return TRUE;
  }

  return FALSE;
}


		 /*******************************
		 *	      ERRORS		*
		 *******************************/

static int
dl_error(const char *op, const char *obj)
{ Sdprintf("Error: %s: %s: %s\n", op, obj, dlerror());

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
{ type_spec tspec = {CT_STRUCT, 0, 0, type, size, free};

  return unify_ptr(t, ptr, 1, &tspec) != NULL;
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

#ifndef RTLD_LAZY
#define RTLD_LAZY 0
#endif
#ifndef RTLD_NOW
#define RTLD_NOW 0
#endif
#ifndef RTLD_GLOBAL
#define RTLD_GLOBAL 0
#endif
#ifndef RTLD_LOCAL
#define RTLD_LOCAL 0
#endif
#ifndef RTLD_NODELETE
#define RTLD_NODELETE 0
#endif
#ifndef RTLD_NOLOAD
#define RTLD_NOLOAD 0
#endif
#ifndef RTLD_DEEPBIND
#define RTLD_DEEPBIND 0
#endif


static foreign_t
ffi_library_create(term_t path, term_t lib, term_t options)
{ char *name;
  int flags = RTLD_LAZY;
  term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();

  while(PL_get_list_ex(tail, head,tail))
  { atom_t opt;

    if ( !PL_get_atom_ex(head, &opt) )
      return FALSE;

    if ( opt == ATOM_lazy )
      flags = (flags & ~(RTLD_LAZY|RTLD_NOW)) | RTLD_LAZY;
    else if ( opt == ATOM_now )
      flags = (flags & ~(RTLD_LAZY|RTLD_NOW)) | RTLD_NOW;
    else if ( opt == ATOM_global )
      flags |= RTLD_GLOBAL;
    else if ( opt == ATOM_local )
      flags |= RTLD_LOCAL;
    else if ( opt == ATOM_nodelete )
      flags |= RTLD_NODELETE;
    else if ( opt == ATOM_noload )
      flags |= RTLD_NOLOAD;
    else if ( opt == ATOM_deepbind )
      flags |= RTLD_DEEPBIND;
  }
  if ( !PL_get_nil_ex(tail) )
    return FALSE;

  if ( PL_get_file_name(path, &name,
			PL_FILE_OSPATH|PL_FILE_SEARCH|PL_FILE_READ) )
  { void *h;

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
  type_spec tspec = {CT_STRUCT, 0, 0, ATOM_c_library};

  if ( get_ptr(lib, &libh, &tspec) )
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
  type_spec tspec = {CT_STRUCT, 0, 0, ATOM_c_library};

  if ( get_ptr(lib, &libh, &tspec) &&
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


static ffi_type *
ffi_type_wchar_t(void)
{ if ( sizeof(wchar_t) == sizeof(int) )
    return &ffi_type_sint;
  else
    return &ffi_type_ushort;			/* Windows; signed or not? */
}

static ffi_type *
ffi_type_size_t(void)
{ if ( sizeof(size_t) == sizeof(long) )
  { return &ffi_type_ulong;
  } else
  { assert(sizeof(size_t) == sizeof(uint64_t));
    return &ffi_type_uint64;			/* Windows; long is 32-bits */
  }
}



static ffi_type *
to_ffi_type(const type_spec *tspec)
{ if ( tspec->ptrl > 0 || (tspec->flags&CTF_OUTPUT) )
    return &ffi_type_pointer;

  switch(tspec->type)
  { case CT_CHAR:	return &ffi_type_schar;
    case CT_UCHAR:	return &ffi_type_uchar;
    case CT_WCHAR_T:	return ffi_type_wchar_t();
    case CT_SHORT:	return &ffi_type_sshort;
    case CT_USHORT:	return &ffi_type_ushort;
    case CT_ENUM:
    case CT_INT:	return &ffi_type_sint;
    case CT_UINT:	return &ffi_type_uint;
    case CT_LONG:	return &ffi_type_slong;
    case CT_ULONG:	return &ffi_type_ulong;
    case CT_SIZE_T:	return ffi_type_size_t();
    case CT_LONGLONG:	return &ffi_type_sint64;
    case CT_ULONGLONG:	return &ffi_type_uint64;
    case CT_FLOAT:	return &ffi_type_float;
    case CT_DOUBLE:	return &ffi_type_double;
    case CT_CLOSURE:	return &ffi_type_pointer;
    default:
      return NULL;
  }
}

static int
get_ffi_type(term_t t, type_spec *pl_type, ffi_type **ffi_type, int isret)
{ if ( !isret && PL_is_functor(t, FUNCTOR_minus1) )
  { term_t t2 = PL_new_term_ref();

    _PL_get_arg(1, t, t2);
    if ( !get_type(t2, pl_type) )
      return FALSE;
    pl_type->flags |= CTF_OUTPUT;
  } else
  { if ( !get_type(t, pl_type) )
      return FALSE;
  }

  if ( !(*ffi_type = to_ffi_type(pl_type)) )
  { if ( isret && pl_type->type == CT_VOID )
      *ffi_type = &ffi_type_void;
    else
      return PL_domain_error("c_type", t);
  }

  return TRUE;
}


static int
get_types(term_t args, type_spec *pl_types, ffi_type **ffi_types)
{ term_t tail = PL_copy_term_ref(args);
  term_t head = PL_new_term_ref();
  int i;

  for(i=0; PL_get_list(tail, head, tail); i++ )
  { if ( !get_ffi_type(head, &pl_types[i], &ffi_types[i], FALSE) )
      return FALSE;
  }

  return TRUE;
}


static void
ci_function_free(void *ptr)
{ ctx_prototype *ctx = ptr;

  if ( ctx )
  { if ( ctx->arg_type )
      free(ctx->arg_type);
    if ( ctx->ffi_type )
      free(ctx->ffi_type);

    free(ctx);
  }
}


/** 'ffi_prototype_create'(+Function, +ABI, +Return, +Params, -Prototype)

Create a function prototype for Function   (a  function pointer).

The Return arg can be a text encoding a  single value as above or one of
the terms pointer(+Type, +Size) or pointer(+Type, +Size +FreeFunc). This
is used to return a  `c_ptr`  blob   for  the  return parameter with the
correct type, element size and  optionally   a  function  to discard the
returned value.

Note that the  `FreeFunc`  is  called   from  the  Prolog  atom  garbage
collector and is thus normally called from the `gc` thread.
*/

static foreign_t
ffi_prototype_create(term_t entry, term_t cc,
		     term_t ret, term_t parms,
		     term_t func)
{ ctx_symbol *ep;
  ffi_abi abi;
  type_spec tspec = {CT_STRUCT, 0, 0, ATOM_c_symbol};
  ctx_prototype *ctx = NULL;
  size_t argc;

  if ( PL_skip_list(parms, 0, &argc) != PL_LIST )
    return PL_type_error("list", parms);

  if ( !(ctx = calloc(1, sizeof(*ctx))) ||
       !(ctx->arg_type = malloc(sizeof(*ctx->arg_type)*argc)) ||
       !(ctx->ffi_type = malloc(sizeof(*ctx->ffi_type)*argc)) )
  { PL_resource_error("memory");
    goto error;
  }
  ctx->argc = argc;

  if ( get_ptr(entry, &ep, &tspec) &&
       get_abi(cc, &abi) &&
       get_ffi_type(ret, &ctx->ret_type, &ctx->ffi_ret, TRUE) &&
       get_types(parms, ctx->arg_type, ctx->ffi_type) )
  { ffi_status rc;

    DEBUG(1, Sdprintf("Prototype args ok (argc=%d)\n", argc));

    rc = ffi_prep_cif(&ctx->cif, abi, ctx->argc, ctx->ffi_ret, ctx->ffi_type);
    if ( rc != FFI_OK )
    { ffi_error(rc);
      goto error;
    }
    ctx->func = ep->func;

    DEBUG(1, Sdprintf("Created prototype\n"));

    if ( unify_part_ptr(func,
			ctx, sizeof(*ctx), ATOM_c_function,
			ci_function_free) )
      return TRUE;
  }

error:
  ci_function_free(ctx);

  return FALSE;
}

typedef union argstore
{ char c;
  unsigned char uc;
  wchar_t wc;
  short s;
  unsigned short us;
  int i;
  unsigned int ui;
  long l;
  unsigned long ul;
  size_t sz;
  int64_t ll;
  uint64_t ull;
  float f;
  double d;
  void *p;
} argstore;


static int
unify_output(term_t t, const type_spec *tp, const argstore *as)
{ if ( tp->ptrl > 0 )
  { type_spec tspec = *tp;

    tspec.ptrl--;
    return unify_ptr(t, as->p, SZ_UNKNOWN, &tspec) != NULL;
  } else
  { switch(tp->type)
    { case CT_VOID:	 return TRUE;
      case CT_CHAR:      return PL_unify_int64 (t, as->c);
      case CT_UCHAR:     return PL_unify_uint64(t, as->uc);
      case CT_WCHAR_T:
	if ( sizeof(wchar_t) == sizeof(int) )
	  return PL_unify_int64 (t, as->i);
	else if ( sizeof(wchar_t) == sizeof(short) )
	  return PL_unify_uint64 (t, as->us);
	else
	  assert(0);
      case CT_SHORT:     return PL_unify_int64 (t, as->s);
      case CT_USHORT:    return PL_unify_uint64(t, as->us);
      case CT_ENUM:
      case CT_INT:       return PL_unify_int64 (t, as->i);
      case CT_UINT:      return PL_unify_uint64(t, as->ui);
      case CT_LONG:      return PL_unify_int64 (t, as->l);
      case CT_ULONG:     return PL_unify_uint64(t, as->ul);
      case CT_SIZE_T:    return PL_unify_uint64(t, as->sz);
      case CT_LONGLONG:  return PL_unify_int64 (t, as->ll);
      case CT_ULONGLONG: return PL_unify_uint64(t, as->ull);
      case CT_FLOAT:     return PL_unify_float (t, as->f);
      case CT_DOUBLE:    return PL_unify_float (t, as->d);
      default:
	assert(0);
        return FALSE;
    }
  }
}


typedef struct oarg
{ argstore     *disp;
  term_t	term;
  int		anum;
} oarg;

static foreign_t
pl_ffi_call(term_t prototype, term_t goal)
{ ctx_prototype *ctx;
  type_spec tspec = {CT_STRUCT, 0, 0, ATOM_c_function};


  if ( get_ptr(prototype, &ctx, &tspec) )
  { void *argv[ctx->argc];
    argstore as[ctx->argc];
    argstore rv;
    int argi;
    term_t arg = PL_new_term_ref();
    oarg oargs[MAX_OUTPUT_ARGS];
    int oarg_count = 0;
    int i;

    for(argi=0; argi<ctx->argc; argi++)
    { const type_spec *t = &ctx->arg_type[argi];

      if ( !PL_get_arg(argi+1, goal, arg) )
      { return ( PL_put_integer(arg, argi+1) &&
		 PL_existence_error("d_arg", arg) );
      }

      if ( (t->flags&CTF_OUTPUT) )
      { if ( oarg_count == MAX_OUTPUT_ARGS )
	  return PL_representation_error("ffi_output_arg_count");

	memset(&as[argi], 0, sizeof(as[argi]));
	oargs[oarg_count].anum = argi;
	oargs[oarg_count].disp = &as[argi];
	oargs[oarg_count].term = PL_copy_term_ref(arg);
	argv[argi] = &oargs[oarg_count].disp;
	oarg_count++;
      } else if ( t->ptrl > 0 )
      { if ( !get_ptr(arg, &as[argi].p, 0) )
	  return FALSE;
	DEBUG(2, Sdprintf("Got ptr %p\n", as[argi].p));
	argv[argi] = &as[argi].p;
      } else
      { switch(t->type)
	{ case CT_CHAR:
	    if ( !PL_cvt_i_char(arg, &as[argi].c) )
	      return FALSE;
	    argv[argi] = &as[argi].c;
	    break;
	  case CT_UCHAR:
	    if ( !PL_cvt_i_uchar(arg, &as[argi].uc) )
	      return FALSE;
	    argv[argi] = &as[argi].uc;
	    break;
	  case CT_WCHAR_T:
	    if ( sizeof(wchar_t) == sizeof(int) )
	    { if ( !PL_cvt_i_int(arg, &as[argi].i) )
		return FALSE;
	      argv[argi] = &as[argi].i;
	    } else if ( sizeof(wchar_t) == sizeof(short) )
	    { if ( !PL_cvt_i_ushort(arg, &as[argi].us) )
		return FALSE;
	      argv[argi] = &as[argi].us;
	    } else
	      assert(0);
	    break;
	  case CT_SHORT:
	    if ( !PL_cvt_i_short(arg, &as[argi].s) )
	      return FALSE;
	    argv[argi] = &as[argi].s;
	    break;
	  case CT_USHORT:
	    if ( !PL_cvt_i_ushort(arg, &as[argi].us) )
	      return FALSE;
	    argv[argi] = &as[argi].us;
	    break;
	  case CT_ENUM:
	  case CT_INT:
	    if ( !PL_cvt_i_int(arg, &as[argi].i) )
	      return FALSE;
	    argv[argi] = &as[argi].i;
	    break;
	  case CT_UINT:
	    if ( !PL_cvt_i_uint(arg, &as[argi].ui) )
	      return FALSE;
	    argv[argi] = &as[argi].ui;
	    break;
	  case CT_LONG:
	    if ( !PL_cvt_i_long(arg, &as[argi].l) )
	      return FALSE;
	    argv[argi] = &as[argi].l;
	    break;
	  case CT_ULONG:
	    if ( !PL_cvt_i_ulong(arg, &as[argi].ul) )
	      return FALSE;
	    argv[argi] = &as[argi].ul;
	    break;
	  case CT_SIZE_T:
	    if ( !PL_cvt_i_size_t(arg, &as[argi].sz) )
	      return FALSE;
	    argv[argi] = &as[argi].sz;
	    break;
	  case CT_LONGLONG:
	    if ( !PL_cvt_i_int64(arg, &as[argi].ll) )
	      return FALSE;
	    argv[argi] = &as[argi].ll;
	    break;
	  case CT_ULONGLONG:
	    if ( !PL_cvt_i_uint64(arg, &as[argi].ull) )
	      return FALSE;
	    argv[argi] = &as[argi].ull;
	    break;
	  case CT_FLOAT:
	    if ( !PL_cvt_i_single(arg, &as[argi].f) )
	      return FALSE;
	    argv[argi] = &as[argi].f;
	    break;
	  case CT_DOUBLE:
	    if ( !PL_cvt_i_float(arg, &as[argi].d) )
	      return FALSE;
	    argv[argi] = &as[argi].d;
	    break;
	  case CT_CLOSURE:
	    if ( !get_closure(arg, &as[argi].p) )
	      return FALSE;
	    argv[argi] = &as[argi].p;
	    break;
	  default:
	    assert(0);
	}
      }
    }

    if ( !(ctx->ret_type.type == CT_VOID && ctx->ret_type.ptrl == 0) )
    { if ( !PL_get_arg(argi+1, goal, arg) )
      { return ( PL_put_integer(arg, argi+1) &&
		 PL_existence_error("d_arg", arg) );
      }
    }

    ffi_call(&ctx->cif, ctx->func, &rv.p, argv);

    for(i=0; i<oarg_count; i++)
    { int ai = oargs[i].anum;
      if ( !unify_output(oargs[i].term,
			 &ctx->arg_type[ai],
			 oargs[i].disp) )
	return FALSE;
    }

    return unify_output(arg, &ctx->ret_type, &rv);
  }

  return FALSE;
}


		 /*******************************
		 *	      CLOSURES		*
		 *******************************/

static void call_closure(ffi_cif *cif, void *ret, void* args[], void *ctxp);

static void
free_closure(ctx_closure *ctx)
{ if ( ctx->ffi_type ) free(ctx->ffi_type);
  if ( ctx->arg_type ) free(ctx->arg_type);

  free(ctx);
}


/** ffi_closure_create(:Predicate, +ABI, +Return, +Params, -Closure)
*/

static foreign_t
ffi_closure_create(term_t qpred,
		   term_t abi, term_t ret, term_t args,
		   term_t closure)
{ module_t m = NULL;
  term_t pred = PL_new_term_ref();
  atom_t pname;
  size_t parity;
  ctx_closure *ctx;
  ffi_abi fabi;
  ffi_type *rtype;

  if ( !(ctx = malloc(sizeof(*ctx))) )
    return PL_resource_error("memory");

  memset(ctx, 0, sizeof(*ctx));

  if ( !PL_strip_module(qpred, &m, pred) ||
       !get_abi(abi, &fabi) )
    goto error;
  if ( !PL_get_name_arity(pred, &pname, &parity) )
  { PL_type_error("callable", qpred);
    goto error;
  }

  ctx->predicate = PL_pred(PL_new_functor(pname, parity), m);
  if ( !get_type(ret, &ctx->ret_type) )
    goto error;
  if ( !(rtype = to_ffi_type(&ctx->ret_type)) )
  { PL_domain_error("c_type", ret);
    goto error;
  }

  if ( PL_skip_list(args, 0, &ctx->argc) != PL_LIST )
  { PL_type_error("list", args);
    goto error;
  }
  if ( !(ctx->arg_type = malloc(sizeof(*ctx->arg_type)*ctx->argc)) ||
       !(ctx->ffi_type = malloc(sizeof(*ctx->ffi_type)*ctx->argc)) )
  { PL_resource_error("memory");
    goto error;
  }

  if ( !get_types(args, ctx->arg_type, ctx->ffi_type) )
    goto error;

  if ( !(ctx->closure = ffi_closure_alloc(sizeof(ffi_closure), &ctx->func)) )
  { PL_resource_error("memory");
    goto error;
  }

  if ( ffi_prep_cif(&ctx->cif, fabi, ctx->argc, rtype, ctx->ffi_type) ==
       FFI_OK )
  { if ( ffi_prep_closure_loc(ctx->closure, &ctx->cif, call_closure,
			      ctx, ctx->func) == FFI_OK )
    { type_spec tspec = {CT_STRUCT, 0, 0, ATOM_c_closure,
			  sizeof(*ctx), free_closure};
      return unify_ptr(closure, ctx, 1, &tspec) != NULL;
    }
  }

  Sdprintf("Error creating closure!\n");

error:
  free_closure(ctx);

  return FALSE;
}


static void
call_closure(ffi_cif *cif, void *ret, void* args[], void *ctxp)
{ ctx_closure *ctx = ctxp;
  int has_ret = ctx->ret_type.type != CT_VOID;
  fid_t fid;

  DEBUG(3, Sdprintf("Calling closure\n"));

  if ( (fid = PL_open_foreign_frame()) )
  { term_t argv;

    if ( (argv=PL_new_term_refs(ctx->argc + has_ret)) )
    { size_t i;
      int rc;

#define BIND_INT(type)    PL_put_integer(argv+i, *(type*)args[i]);
#define BIND_INT64(type)  PL_put_int64(argv+i, *(type*)args[i]);
#define BIND_UINT64(type) PL_unify_uint64(argv+i, *(type*)args[i]);
#define BIND_FLOAT(type)  PL_put_float(argv+i, *(type*)args[i]);

      for(i=0; i<ctx->argc; i++)
      { const type_spec *tspec = &ctx->arg_type[i];

	if ( tspec->ptrl > 0 )
	{ type_spec vtype = *tspec;

	  vtype.ptrl--;
	  rc = unify_ptr(argv+i, *(void**)args[i], SZ_UNKNOWN, &vtype) != NULL;
	} else
	{ switch(tspec->type)
	  { case CT_CHAR:	     rc = BIND_INT(char);            break;
	    case CT_UCHAR:     rc = BIND_INT(unsigned char);         break;
	    case CT_WCHAR_T:   rc = BIND_INT(wchar_t);               break;
	    case CT_SHORT:     rc = BIND_INT(short);                 break;
	    case CT_USHORT:    rc = BIND_INT(unsigned short);        break;
	    case CT_INT:       rc = BIND_INT(int);                   break;
	    case CT_UINT:      rc = BIND_INT(unsigned int);          break;
	    case CT_LONG:      rc = BIND_INT(long);                  break;
	    case CT_ULONG:     rc = BIND_INT64(unsigned long);       break;
	    case CT_SIZE_T:    rc = BIND_INT64(size_t);		     break;
	    case CT_LONGLONG:  rc = BIND_INT64(long long);           break;
	    case CT_ULONGLONG: rc = BIND_UINT64(unsigned long long); break;
	    case CT_FLOAT:     rc = BIND_FLOAT(float);               break;
	    case CT_DOUBLE:    rc = BIND_FLOAT(double);              break;
	    default:
	      assert(0);				/* TBD: pointers */
	      rc = 0;
	  }
	}

	if ( !rc )
	  Sdprintf("Closure: failed to convert arg %d\n", i+1);
	DEBUG(4, PL_write_term(Serror, argv+i, 1200, PL_WRT_NEWLINE));
      }

      if ( PL_call_predicate(NULL, PL_Q_NORMAL, ctx->predicate, argv) )
      { if ( has_ret )
	{ term_t rt = argv+ctx->argc;

	  DEBUG(4, PL_write_term(Serror, rt, 1200, PL_WRT_NEWLINE));

	  if ( ctx->ret_type.ptrl > 0 )
	  { rc = get_ptr(rt, ret, 0);
	  } else
	  { switch(ctx->ret_type.type)
	    { case CT_CHAR:      rc = PL_cvt_i_char(rt, ret);   break;
	      case CT_UCHAR:     rc = PL_cvt_i_uchar(rt, ret);  break;
	      case CT_WCHAR_T:   rc = PL_cvt_i_wchar(rt, ret);  break;
	      case CT_SHORT:     rc = PL_cvt_i_short(rt, ret);  break;
	      case CT_USHORT:    rc = PL_cvt_i_ushort(rt, ret); break;
	      case CT_INT:       rc = PL_cvt_i_int(rt, ret);    break;
	      case CT_UINT:      rc = PL_cvt_i_uint(rt, ret);   break;
	      case CT_LONG:      rc = PL_cvt_i_long(rt, ret);   break;
	      case CT_ULONG:     rc = PL_cvt_i_ulong(rt, ret);  break;
	      case CT_SIZE_T:    rc = PL_cvt_i_size_t(rt, ret); break;
	      case CT_LONGLONG:  rc = PL_cvt_i_int64(rt, ret);  break;
	      case CT_ULONGLONG: rc = PL_cvt_i_uint64(rt, ret); break;
	      case CT_FLOAT:     rc = PL_cvt_i_single(rt, ret); break;
	      case CT_DOUBLE:    rc = PL_cvt_i_float(rt, ret);  break;
	      default:
		assert(0);
	        rc = 0;
	    }
	  }

	  if ( !rc )
	    Sdprintf("Closure: failed to convert return value\n");
	}
      }
    }

    PL_close_foreign_frame(fid);
  }
}


static int
get_closure(term_t t, void **func)
{ ctx_closure *ctx;
  type_spec tspec = {CT_STRUCT, 0, 0, ATOM_c_closure};

  if ( get_ptr(t, &ctx, &tspec) )
  { *func = ctx->func;

    return TRUE;
  }

  return FALSE;
}

#define TEST_CLOSURE 1
#if TEST_CLOSURE

static foreign_t
i_ii_closure(term_t closure, term_t I1, term_t I2, term_t R)
{ void *func;

  if ( get_closure(closure, &func) )
  { int i1, i2;

    if ( PL_cvt_i_int(I1, &i1) &&
	 PL_cvt_i_int(I2, &i2) )
    { int (*f)(int,int) = func;
      int r;

      r = (*f)(i1,i2);
      return PL_unify_integer(R, r);
    }
  }

  return FALSE;
}



#endif /*TEST_CLOSURE*/

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
  MKATOM(c_closure);

  MKATOM(default);
  MKATOM(cdecl);
  MKATOM(stdcall);
  MKATOM(fastcall);

  MKATOM(lazy);
  MKATOM(now);
  MKATOM(global);
  MKATOM(local);
  MKATOM(nodelete);
  MKATOM(noload);
  MKATOM(deepbind);

  MKATOM(void);

  FUNCTOR_minus1 = PL_new_functor(PL_new_atom("-"), 1);

  install_c_memory();

  PL_register_foreign("ffi_library_create",   3, ffi_library_create,   0);
  PL_register_foreign("ffi_library_free",     1, pl_ffi_library_free,  0);
  PL_register_foreign("ffi_lookup_symbol",    3, ffi_lookup_symbol,    0);
  PL_register_foreign("ffi_prototype_create", 5, ffi_prototype_create, 0);
  PL_register_foreign("ffi_call",	      2, pl_ffi_call,	       0);

  PL_register_foreign("ffi_closure_create",   5, ffi_closure_create,
		      PL_FA_META, "0+++-");
#ifdef TEST_CLOSURE
  PL_register_foreign("i_ii_closure",	      4, i_ii_closure,         0);
#endif

  PL_register_foreign("ffi_debug",	      1, ffi_debug,	       0);
  PL_register_foreign("c_errno",	      1, c_errno,	       0);
}
