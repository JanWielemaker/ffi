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

#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <cinvoke.h>
#include <assert.h>

static int debug = 1;

#define DEBUG(g) if ( debug ) do { g; } while(0)

static atom_t ATOM_ci_context;
static atom_t ATOM_ci_library;
static atom_t ATOM_ci_function;
static atom_t ATOM_ci_prototype;
static atom_t ATOM_cdecl;
static atom_t ATOM_stdcall;
static atom_t ATOM_fastcall;

typedef struct ci_ptr
{ void *ptr;					/* the pointer */
  atom_t type;					/* Its type */
  void (*free)(void *ptr);			/* Its free function */
} ci_ptr;


static int
write_ci_ptr(IOSTREAM *s, atom_t aref, int flags)
{ ci_ptr *ref = PL_blob_data(aref, NULL, NULL);
  (void)flags;

  Sfprintf(s, "<ci_ptr>(%s,%p)", PL_atom_chars(ref->type), ref->ptr);
  return TRUE;
}


static void
acquire_ci_ptr(atom_t aref)
{ ci_ptr *ref = PL_blob_data(aref, NULL, NULL);
  (void)ref;
}


static int
release_ci_ptr(atom_t aref)
{ ci_ptr *ref = PL_blob_data(aref, NULL, NULL);

  if ( ref->free && ref->ptr )
    (*ref->free)(ref->ptr);

  return TRUE;
}


static int
save_ci_ptr(atom_t aref, IOSTREAM *fd)
{ ci_ptr *ref = PL_blob_data(aref, NULL, NULL);
  (void)fd;

  return PL_warning("Cannot save reference to <ci_ptr>(%s,%p)",
		    PL_atom_chars(ref->type), ref->ptr);
}


static atom_t
load_ci_ptr(IOSTREAM *fd)
{ (void)fd;

  return PL_new_atom("<ci_ptr>");
}


static PL_blob_t ci_ptr_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE,
  "ci_ptr",
  release_ci_ptr,
  NULL,
  write_ci_ptr,
  acquire_ci_ptr,
  save_ci_ptr,
  load_ci_ptr
};


static int
unify_ptr(term_t t, void *ptr, atom_t type)
{ ci_ptr ref;

  ref.ptr  = ptr;
  ref.type = type;
  ref.free = NULL;

  return PL_unify_blob(t, &ref, sizeof(ref), &ci_ptr_blob);
}


static int
get_ptr(term_t t, void *ptrp, atom_t ptrtype)
{ PL_blob_t *type;
  void *bp;

  if ( PL_get_blob(t, &bp, NULL, &type) &&
       type == &ci_ptr_blob )
  { ci_ptr *ref = bp;
    void **ptrpp = ptrp;

    *ptrpp = ref->ptr;
    return TRUE;
  }

  return FALSE;
}


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
		 *		API		*
		 *******************************/


static foreign_t
ci_context_create(term_t ctx)
{ CInvContext *cictx;

  if ( (cictx=cinv_context_create()) )
    return unify_ptr(ctx, cictx, ATOM_ci_context);

  return FALSE;
}


static foreign_t
ci_library_create(term_t ctx, term_t path, term_t lib)
{ char *name;
  CInvContext *cictx;

  if ( get_ptr(ctx, &cictx, ATOM_ci_context) &&
       PL_get_file_name(path, &name,
			PL_FILE_OSPATH|PL_FILE_SEARCH|PL_FILE_READ) )
  { CInvLibrary *h;

    DEBUG(Sdprintf("Opening %s\n", name));

    if ( (h=cinv_library_create(cictx, name)) )
      return unify_ptr(lib, h, ATOM_ci_library);
  }

  return FALSE;
}


static foreign_t
ci_free_library(term_t ctx, term_t lib)
{ CInvContext *cictx;
  CInvLibrary *libh;

  if ( get_ptr(ctx, &cictx, ATOM_ci_context) &&
       get_ptr(lib, &libh, ATOM_ci_library) )
  { cinv_status_t rc = cinv_library_delete(cictx, libh);

    return ci_status(rc, cictx);
  }

  return FALSE;
}


static foreign_t
ci_library_load_entrypoint(term_t ctx, term_t lib, term_t name, term_t func)
{ CInvContext *cictx;
  CInvLibrary *libh;
  char *fname;

  if ( get_ptr(ctx, &cictx, ATOM_ci_context) &&
       get_ptr(lib, &libh, ATOM_ci_library) &&
       PL_get_chars(name, &fname, CVT_ATOM|CVT_EXCEPTION) )
  { void *f;

    DEBUG(Sdprintf("Find %s in %p\n", fname, libh));

    if ( (f=cinv_library_load_entrypoint(cictx, libh, fname)) )
      return unify_ptr(func, f, ATOM_ci_function);
  }

  return FALSE;
}


static foreign_t
ci_function_create(term_t ctx, term_t cc, term_t ret, term_t parms, term_t func)
{ CInvContext *cictx;
  cinv_callconv_t ccv;
  char *rformat;
  char *pformat;

  if ( get_ptr(ctx, &cictx, ATOM_ci_context) &&
       get_cc(cc, &ccv) &&
       PL_get_chars(ret, &rformat, CVT_ATOM|CVT_EXCEPTION) &&
       PL_get_chars(parms, &pformat, CVT_ATOM|CVT_EXCEPTION) )
  { CInvFunction *f;

    if ( (f=cinv_function_create(cictx, ccv, rformat, pformat)) )
      return unify_ptr(func, f, ATOM_ci_prototype);
    return ci_error(cictx);
  }

  return FALSE;
}


static foreign_t
ci_function_invoke(term_t ctx, term_t prototype, term_t fptr,
		   term_t goal)
{
}




#define MKATOM(n) \
        ATOM_ ## n = PL_new_atom(#n)
#define MKFUNCTOR(n,a) \
        FUNCTOR_ ## n ## a = PL_new_functor(PL_new_atom(#n), a)

install_t
install(void)
{ MKATOM(ci_context);
  MKATOM(ci_library);
  MKATOM(ci_function);
  MKATOM(ci_prototype);
  MKATOM(cdecl);
  MKATOM(stdcall);
  MKATOM(fastcall);

  PL_register_foreign("ci_context_create",  1, ci_context_create,  0);
  PL_register_foreign("ci_library_create",  3, ci_library_create,  0);
  PL_register_foreign("ci_free_library",    2, ci_free_library,    0);
  PL_register_foreign("ci_library_load_entrypoint",
					    4, ci_library_load_entrypoint, 0);
  PL_register_foreign("ci_function_create", 5, ci_function_create, 0);
  PL_register_foreign("ci_function_invoke", 4, ci_function_invoke, 0);
}
