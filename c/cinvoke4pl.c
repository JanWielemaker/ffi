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
#include <dynload.h>
#include <dyncall.h>
#include <assert.h>
#include <pthread.h>

static int debug = 1;
static pthread_key_t key;

#define DEBUG(g) if ( debug ) do { g; } while(0)

static atom_t ATOM_dc_library;
static atom_t ATOM_dc_func;

typedef struct dc_ptr
{ void *ptr;					/* the pointer */
  atom_t type;					/* Its type */
  void (*free)(void *ptr);			/* Its free function */
} dc_ptr;


static int
write_dc_ptr(IOSTREAM *s, atom_t aref, int flags)
{ dc_ptr *ref = PL_blob_data(aref, NULL, NULL);
  (void)flags;

  Sfprintf(s, "<dc_ptr>(%s,%p)", PL_atom_chars(ref->type), ref->ptr);
  return TRUE;
}


static void
acquire_dc_ptr(atom_t aref)
{ dc_ptr *ref = PL_blob_data(aref, NULL, NULL);
  (void)ref;
}


static int
release_dc_ptr(atom_t aref)
{ dc_ptr *ref = PL_blob_data(aref, NULL, NULL);

  if ( ref->free && ref->ptr )
    (*ref->free)(ref->ptr);

  return TRUE;
}


static int
save_dc_ptr(atom_t aref, IOSTREAM *fd)
{ dc_ptr *ref = PL_blob_data(aref, NULL, NULL);
  (void)fd;

  return PL_warning("Cannot save reference to <dc_ptr>(%s,%p)",
		    PL_atom_chars(ref->type), ref->ptr);
}


static atom_t
load_dc_ptr(IOSTREAM *fd)
{ (void)fd;

  return PL_new_atom("<dc_ptr>");
}


static PL_blob_t dc_ptr_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE,
  "dc_ptr",
  release_dc_ptr,
  NULL,
  write_dc_ptr,
  acquire_dc_ptr,
  save_dc_ptr,
  load_dc_ptr
};


static int
unify_ptr(term_t t, void *ptr, atom_t type)
{ dc_ptr ref;

  ref.ptr  = ptr;
  ref.type = type;
  ref.free = NULL;

  return PL_unify_blob(t, &ref, sizeof(ref), &dc_ptr_blob);
}


static int
get_ptr(term_t t, void **ptrp, atom_t ptrtype)
{ PL_blob_t *type;
  void *bp;

  if ( PL_get_blob(t, &bp, NULL, &type) &&
       type == &dc_ptr_blob )
  { dc_ptr *ref = bp;

    *ptrp = ref->ptr;
    return TRUE;
  }

  return FALSE;
}


		 /*******************************
		 *		API		*
		 *******************************/


static foreign_t
dc_load_library(term_t file, term_t ptr)
{ char *name;

  if ( PL_get_file_name(file, &name, PL_FILE_OSPATH|PL_FILE_SEARCH|PL_FILE_READ) )
  { void *h;

    DEBUG(Sdprintf("Opening %s\n", name));

    if ( (h=dlLoadLibrary(name)) )
      return unify_ptr(ptr, h, ATOM_dc_library);
  }

  return FALSE;
}


static foreign_t
dc_free_library(term_t h)
{ void *ptr;

  if ( get_ptr(h, &ptr, ATOM_dc_library) )
  { dlFreeLibrary(ptr);

    return TRUE;
  }

  return FALSE;
}


static foreign_t
dc_find_symbol(term_t lib, term_t name, term_t func)
{ void *libh;
  char *fname;

  if ( get_ptr(lib, &libh, ATOM_dc_library) &&
       PL_get_chars(name, &fname, CVT_ATOM|CVT_EXCEPTION) )
  { void *f;

    DEBUG(Sdprintf("Find %s in %p\n", fname, libh));

    if ( (f=dlFindSymbol(libh, fname)) )
      return unify_ptr(func, f, ATOM_dc_func);
  }

  return FALSE;
}


static foreign_t
dc_call(term_t function, term_t signature, term_t goal)
{ void *fptr;
  char *sig;

  if ( get_ptr(function, &fptr, ATOM_dc_func) &&
       PL_get_chars(signature, &sig, CVT_ATOM|CVT_STRING|CVT_EXCEPTION) )
  { int i;
    DCCallVM *vm;
    term_t arg = PL_new_term_ref();
    int mode = 1;			/* input */
    int rc;

    if ( !(vm=pthread_getspecific(key)) )
    { vm = dcNewCallVM(4096);
      DEBUG(Sdprintf("Created vm at %p\n", vm));
      dcMode(vm, DC_CALL_C_DEFAULT);
      pthread_setspecific(key, vm);
    } else
    { //DEBUG(Sdprintf("Reusing vm at %p\n", vm));
      dcReset(vm);
    }

    if ( *sig == '(' )
      sig++;

    for(i=0; ; i++)
    { if ( !PL_get_arg(i+mode, goal, arg) )
      { rc = ( PL_put_integer(arg, i+mode) &&
	       PL_existence_error("d_arg", arg) );
	goto error;
      }

      if ( mode == 1 )
      { switch(sig[i])
	{ case 'd':
	  { double d;

	    if ( !(rc=PL_cvt_i_float(arg, &d)) )
	      goto error;
	    dcArgDouble(vm, d);
	    break;
	  }
	  case ')':
	    mode = 0;			/* output */
	    break;
          default:
	    rc = PL_syntax_error("dc_signature", NULL);
	    goto error;
	}
      } else
      { switch(sig[i])
	{ case 'd':
	  { double d = dcCallDouble(vm, fptr);
	    rc = PL_cvt_o_float(d, arg);
	    break;
	  }
          default:
	    rc = PL_syntax_error("dc_signature", NULL);
	    goto error;
	}
	break;
      }
    }

  error:
    return rc;
  }

  return FALSE;
}


static void
dc_cleanup(void *ctx)
{ dcFree(ctx);
}


install_t
install(void)
{ ATOM_dc_library = PL_new_atom("dc_library");
  ATOM_dc_func    = PL_new_atom("dc_function");

  PL_register_foreign("dc_load_library", 2, dc_load_library, 0);
  PL_register_foreign("dc_free_library", 1, dc_free_library, 0);
  PL_register_foreign("dc_find_symbol",  3, dc_find_symbol,  0);
  PL_register_foreign("dc_call",         3, dc_call,         0);

  pthread_key_create(&key, dc_cleanup);
}
