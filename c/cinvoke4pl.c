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

static int debug = 1;

#define DEBUG(g) if ( debug ) do { g; } while(0)

static atom_t ATOM_ci_context;
static atom_t ATOM_ci_library;
static atom_t ATOM_ci_function;
static atom_t ATOM_ci_prototype;
static atom_t ATOM_ci_struct;
static atom_t ATOM_ci_struct_decl;

static atom_t ATOM_cdecl;
static atom_t ATOM_stdcall;
static atom_t ATOM_fastcall;

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

typedef struct ctx_prototype
{ CInvContext *cictx;
  void *entrypoint;
  const char *rformat;
  const char *pformat;
} ctx_prototype;


		 /*******************************
		 *		API		*
		 *******************************/


static foreign_t
ci_context_create(term_t ctx)
{ CInvContext *cictx;

  if ( (cictx=cinv_context_create()) )
    return unify_ptr(ctx, cictx, NULL, ATOM_ci_context);

  return FALSE;
}


static foreign_t
ci_library_create(term_t ctx, term_t path, term_t lib)
{ char *name;
  CInvContext *cictx;

  if ( get_ptr(ctx, &cictx, NULL, ATOM_ci_context) &&
       PL_get_file_name(path, &name,
			PL_FILE_OSPATH|PL_FILE_SEARCH|PL_FILE_READ) )
  { CInvLibrary *h;

    DEBUG(Sdprintf("Opening %s\n", name));

    if ( (h=cinv_library_create(cictx, name)) )
      return unify_ptr(lib, h, cictx, ATOM_ci_library);

    return ci_error(cictx);
  }

  return FALSE;
}


static foreign_t
ci_free_library(term_t lib)
{ CInvContext *cictx;
  CInvLibrary *libh;

  if ( get_ptr(lib, &libh, &cictx, ATOM_ci_library) )
  { cinv_status_t rc = cinv_library_delete(cictx, libh);

    return ci_status(rc, cictx);
  }

  return FALSE;
}


static foreign_t
ci_library_load_entrypoint(term_t lib, term_t name, term_t func)
{ CInvContext *cictx;
  CInvLibrary *libh;
  char *fname;

  if ( get_ptr(lib, &libh, &cictx, ATOM_ci_library) &&
       PL_get_chars(name, &fname, CVT_ATOM|CVT_EXCEPTION) )
  { void *f;

    DEBUG(Sdprintf("Find %s in %p\n", fname, libh));

    if ( (f=cinv_library_load_entrypoint(cictx, libh, fname)) )
      return unify_ptr(func, f, cictx, ATOM_ci_function);
  }

  return FALSE;
}


static foreign_t
ci_function_create(term_t entry, term_t cc, term_t ret, term_t parms, term_t func)
{ void *entrypoint;
  CInvContext *cictx;
  cinv_callconv_t ccv;
  char *rformat;
  char *pformat;

  if ( get_ptr(entry, &entrypoint, &cictx, ATOM_ci_function) &&
       get_cc(cc, &ccv) &&
       PL_get_chars(ret, &rformat, CVT_ATOM|CVT_STRING|CVT_EXCEPTION) &&
       PL_get_chars(parms, &pformat, CVT_ATOM|CVT_STRING|CVT_EXCEPTION) )
  { CInvFunction *f;

    if ( pformat[0] == '(' )
      pformat++;			/* dyncall compatibility */

    if ( (f=cinv_function_create(cictx, ccv, rformat, pformat)) )
    { ctx_prototype *p = malloc(sizeof(*p));

      if ( p )
      { memset(p, 0, sizeof(p));
	p->cictx = cictx;
	p->entrypoint = entrypoint;
	p->rformat    = strdup(rformat);
	p->pformat    = strdup(pformat);

	return unify_ptr(func, f, p, ATOM_ci_prototype);
      } else
      { return PL_resource_error("memory");
      }
    }
    return ci_error(cictx);
  }

  return FALSE;
}


#define MAXARGC 10

typedef union argstore
{ char c;
  short s;
  int i;
  long l;
  long long e;
  float f;
  double d;
  void *p;
  int16_t i2;
  int32_t i4;
  int64_t i8;
} argstore;

static foreign_t
ci_function_invoke(term_t prototype, term_t goal)
{ ctx_prototype *ctx;
  CInvFunction *f;

  if ( get_ptr(prototype, &f, &ctx, ATOM_ci_prototype) )
  { void *argv[MAXARGC];
    argstore as[MAXARGC];
    argstore rv;
    int argc = 0;
    term_t arg = PL_new_term_ref();
    const char *pfmt;

    for(pfmt = ctx->pformat; *pfmt; pfmt++, argc++)
    { if ( !PL_get_arg(argc+1, goal, arg) )
      { return ( PL_put_integer(arg, argc+1) &&
		 PL_existence_error("d_arg", arg) );
      }

      switch(*pfmt)
      { case 'c':
	  if ( !PL_cvt_i_char(arg, &as[argc].c) )
	    return FALSE;
	  argv[argc] = &as[argc].c;
	  break;
	case 's':
	case '2':
	  if ( !PL_cvt_i_short(arg, &as[argc].s) )
	    return FALSE;
	  argv[argc] = &as[argc].s;
	  break;
	case 'i':
	case '4':
	  if ( !PL_cvt_i_int(arg, &as[argc].i) )
	    return FALSE;
	  argv[argc] = &as[argc].i;
	  break;
	case 'l':
	  if ( !PL_cvt_i_long(arg, &as[argc].l) )
	    return FALSE;
	  argv[argc] = &as[argc].l;
	  break;
	case 'e':
	case '8':
	{ int64_t e;
	  if ( !PL_cvt_i_int64(arg, &e) )
	    return FALSE;
	  as[argc].e = e;
	  argv[argc] = &as[argc].e;
	  break;
	}
	case 'f':
	  if ( !PL_cvt_i_single(arg, &as[argc].f) )
	    return FALSE;
	  argv[argc] = &as[argc].f;
	  break;
	case 'd':
	  if ( !PL_cvt_i_float(arg, &as[argc].d) )
	    return FALSE;
	  argv[argc] = &as[argc].d;
	  break;
      }
    }

    if ( ctx->rformat && ctx->rformat[0] )
    { if ( !PL_get_arg(argc+1, goal, arg) )
      { return ( PL_put_integer(arg, argc+1) &&
		 PL_existence_error("d_arg", arg) );
      }
    }

    cinv_function_invoke(ctx->cictx, f, ctx->entrypoint,
			 &rv.p, argv);

    if ( ctx->rformat && ctx->rformat[0] )
    { switch(ctx->rformat[0])
      { case 'c': return PL_cvt_o_int64(rv.c, arg);
        case '2':
        case 's': return PL_cvt_o_int64(rv.s, arg);
        case '4':
        case 'i': return PL_cvt_o_int64(rv.i, arg);
        case 'l': return PL_cvt_o_int64(rv.l, arg);
        case '8':
        case 'e': return PL_cvt_o_int64(rv.e, arg);
        case 'f': return PL_cvt_o_float(rv.f, arg);
        case 'd': return PL_cvt_o_float(rv.d, arg);
      }
    } else
    { return TRUE;			/* void function */
    }
  }

  return FALSE;
}


		 /*******************************
		 *	     STRUCTURES		*
		 *******************************/

typedef struct ctx_member
{ atom_t	name;
  cinv_type_t	type;
  struct ctx_member *next;
} ctx_member;


typedef struct ctx_structure
{ CInvContext   *cictx;
  CInvStructure *structure;
  atom_t         name;
  ctx_member    *members;
  int	         finished;
} ctx_structure;


static int
get_atom_and_string(term_t t, atom_t *aname, const char **sname)
{ if ( PL_get_atom_ex(t, aname) )
  { *sname = PL_atom_chars(*aname);
    return TRUE;
  }

  return FALSE;
}


static int
struct_add_member(ctx_structure *def, const char *name, cinv_type_t type)
{ ctx_member *m = malloc(sizeof(*m));

  if ( m )
  { ctx_member **mp;

    m->name = PL_new_atom(name);
    m->type = type;
    m->next = NULL;

    for(mp = &def->members; *mp; mp = &(*mp)->next)
      ;
    *mp = m;

    return TRUE;
  }

  return PL_resource_error("memory");
}


static ctx_member *
struct_find_member(ctx_structure *def, term_t t, atom_t name)
{ ctx_member *m;

  for(m=def->members; m; m=m->next)
  { if ( m->name == name )
      return m;
  }

  PL_existence_error("member", t);
  return NULL;
}


static foreign_t
ci_structure_create(term_t ctx, term_t name, term_t sptr)
{ CInvContext *cictx;
  atom_t aname;

  if ( get_ptr(ctx, &cictx, NULL, ATOM_ci_context) &&
       PL_get_atom_ex(name, &aname) )
  { ctx_structure *def = malloc(sizeof(*def));

    if ( def )
    { CInvStructure *cs;

      memset(def, 0, sizeof(def));

      def->cictx = cictx;
      def->name = aname;
      PL_register_atom(aname);

      if ( (cs=cinv_structure_create(cictx)) )
      { def->structure = cs;
	return unify_ptr(sptr, cs, def, ATOM_ci_struct_decl);
      }
      return ci_error(cictx);
    } else
    { return PL_resource_error("memory");
    }
  }

  return FALSE;
}


static foreign_t
ci_structure_addmember_value(term_t structure, term_t name, term_t type)
{ ctx_structure *def;
  CInvStructure *cs;
  char *fname;
  cinv_type_t etype;

  if ( get_ptr(structure, &cs, &def, ATOM_ci_struct_decl) &&
       PL_get_chars(name, &fname, CVT_ATOM|CVT_STRING|CVT_EXCEPTION) &&
       get_type(type, &etype) )
  { cinv_status_t rc;

    if ( !struct_add_member(def, fname, etype) )
      return FALSE;
    rc = cinv_structure_addmember_value(def->cictx, cs, fname, etype);
    return ci_status(rc, def->cictx);
  }

  return FALSE;
}


static foreign_t
ci_structure_finish(term_t structure)
{ ctx_structure *def;
  CInvStructure *cs;

  if ( get_ptr(structure, &cs, &def, ATOM_ci_struct_decl) )
  { cinv_status_t rc;

    rc = cinv_structure_finish(def->cictx, cs);
    if ( rc == CINV_SUCCESS )
      def->finished = TRUE;
    return ci_status(rc, def->cictx);
  }

  return FALSE;
}


static foreign_t
ci_structure_create_instance(term_t structure, term_t inst)
{ ctx_structure *def;
  CInvStructure *cs;

  if ( get_ptr(structure, &cs, &def, ATOM_ci_struct_decl) )
  { void *ptr;

    if ( (ptr=cinv_structure_create_instance(def->cictx, cs)) )
    { return unify_ptr(inst, ptr, def, ATOM_ci_struct);
    }

    return ci_error(def->cictx);
  }

  return FALSE;
}


static foreign_t
ci_structure_instance_setvalue(term_t inst, term_t name, term_t value)
{ ctx_structure *def;
  void *ptr;
  atom_t aname;
  const char *sname;
  ctx_member *m;

  if ( get_ptr(inst, &ptr, &def, ATOM_ci_struct) &&
       get_atom_and_string(name, &aname, &sname) &&
       (m=struct_find_member(def, name, aname)) )
  { cinv_status_t status;
    argstore store;
    void *vptr;

    switch(m->type)
    { case CINV_T_CHAR:
	if ( !PL_cvt_i_char(value, &store.c) ) return FALSE;
        vptr = &store.c;
	break;
      case CINV_T_SHORT:
	if ( !PL_cvt_i_short(value, &store.s) ) return FALSE;
        vptr = &store.s;
	break;
      case CINV_T_INT:
	if ( !PL_cvt_i_int(value, &store.i) ) return FALSE;
        vptr = &store.i;
	break;
      case CINV_T_LONG:
	if ( !PL_cvt_i_long(value, &store.l) ) return FALSE;
        vptr = &store.l;
	break;
      case CINV_T_EXTRALONG:
      { int64_t e;
	if ( !PL_cvt_i_int64(value, &e) ) return FALSE;
	store.e = e;
        vptr = &store.e;
	break;
      }
      case CINV_T_FLOAT:
	if ( !PL_cvt_i_single(value, &store.f) ) return FALSE;
        vptr = &store.f;
	break;
      case CINV_T_DOUBLE:
	if ( !PL_cvt_i_float(value, &store.d) ) return FALSE;
        vptr = &store.d;
	break;
      case CINV_T_PTR:
	assert(0);
        break;
    }

    status = cinv_structure_instance_setvalue(
		 def->cictx,
		 def->structure,
		 ptr,
		 sname,
		 vptr);

    return ci_status(status, def->cictx);
  }

  return FALSE;
}


static foreign_t
ci_structure_instance_getvalue(term_t inst, term_t name, term_t value)
{ ctx_structure *def;
  void *ptr;
  atom_t aname;
  const char *sname;
  ctx_member *m;

  if ( get_ptr(inst, &ptr, &def, ATOM_ci_struct) &&
       get_atom_and_string(name, &aname, &sname) &&
       (m=struct_find_member(def, name, aname)) )
  { void *vptr;

    if ( (vptr=cinv_structure_instance_getvalue(
		   def->cictx,
		   def->structure,
		   ptr,
		   sname)) )
    { switch(m->type)
      { case CINV_T_CHAR:
	{ char *vp = vptr;
	  return PL_cvt_o_int64(*vp, value);
	}
	case CINV_T_SHORT:
	{ short *vp = vptr;
	  return PL_cvt_o_int64(*vp, value);
	}
	case CINV_T_INT:
	{ int *vp = vptr;
	  return PL_cvt_o_int64(*vp, value);
	}
	case CINV_T_LONG:
	{ long *vp = vptr;
	  return PL_cvt_o_int64(*vp, value);
	}
	case CINV_T_EXTRALONG:
	{ int64_t *vp = vptr;
	  return PL_cvt_o_int64(*vp, value);
	}
	case CINV_T_FLOAT:
	{ float *vp = vptr;
	  return PL_cvt_o_float(*vp, value);
	}
	case CINV_T_DOUBLE:
	{ double *vp = vptr;
	  return PL_cvt_o_float(*vp, value);
	}
	case CINV_T_PTR:
	  assert(0);
	  return FALSE;
      }
    }

    return ci_error(def->cictx);
  }

  return FALSE;
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
  MKATOM(ci_function);
  MKATOM(ci_prototype);
  MKATOM(ci_struct);
  MKATOM(ci_struct_decl);

  MKATOM(cdecl);
  MKATOM(stdcall);
  MKATOM(fastcall);

  install_c_memory();

  PL_register_foreign("ci_context_create",  1, ci_context_create,  0);
  PL_register_foreign("ci_library_create",  3, ci_library_create,  0);
  PL_register_foreign("ci_free_library",    1, ci_free_library,    0);
  PL_register_foreign("ci_library_load_entrypoint",
					    3, ci_library_load_entrypoint, 0);
  PL_register_foreign("ci_function_create", 5, ci_function_create, 0);
  PL_register_foreign("ci_function_invoke", 2, ci_function_invoke, 0);

  PL_register_foreign("ci_structure_create", 3, ci_structure_create, 0);
  PL_register_foreign("ci_structure_addmember_value",
					    3, ci_structure_addmember_value, 0);
  PL_register_foreign("ci_structure_finish", 2, ci_structure_finish, 0);
  PL_register_foreign("ci_structure_create_instance",
					    2, ci_structure_create_instance, 0);
  PL_register_foreign("ci_structure_instance_setvalue",
					    3, ci_structure_instance_setvalue, 0);
  PL_register_foreign("ci_structure_instance_getvalue",
					    3, ci_structure_instance_getvalue, 0);
}
