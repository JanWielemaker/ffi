/*
C/Invoke Source Code File

Copyright (c) 2006 Will Weisser

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.
   2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation
and/or other materials provided with the distribution.
   3. The name of the author may not be used to endorse or promote products
derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
*/
#include <lua.h>
#include <cinvoke.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#ifdef _MSC_VER
#define snprintf _snprintf
#endif

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

// stack reading functions, we define our own so we can specify
// the exact semantics/errors we want
void expected(lua_State *l, const char *exp, int index) {
	char err[256];
	snprintf(err, sizeof(err), "expected %s, got %s",
		exp, lua_typename(l, index));
	lua_pushstring(l, err);
	lua_error(l);
}

const char *tolstring(lua_State *l, int index, size_t *len) {
	if (lua_type(l, index) != LUA_TSTRING)
		expected(l, "string", index);
	return lua_tolstring(l, index, len);
}
const char *tostring(lua_State *l, int index) {
	return tolstring(l, index, NULL);
}
void *touserdata(lua_State *l, int index) {
	if (lua_type(l, index) != LUA_TUSERDATA)
		expected(l, "userdata", index);
	return lua_touserdata(l, index);
}
void *tolightuserdata(lua_State *l, int index) {
	if (lua_type(l, index) != LUA_TLIGHTUSERDATA)
		expected(l, "lightuserdata", index);
	return lua_touserdata(l, index);
}
lua_Integer tointeger(lua_State *l, int index) {
	if (lua_type(l, index) != LUA_TNUMBER)
		expected(l, "integer", index);
	return lua_tointeger(l, index);
}
lua_Number tonumber(lua_State *l, int index) {
	if (lua_type(l, index) != LUA_TNUMBER)
		expected(l, "number", index);
	return lua_tonumber(l, index);
}
int toboolean(lua_State *l, int index) {
	if (lua_type(l, index) != LUA_TBOOLEAN)
		expected(l, "boolean", index);
	return lua_toboolean(l, index);
}

struct LibStruct {
	CInvContext *ctx;
	CInvLibrary *lib;
	cinv_callconv_t cc;
};

int _clibrary_new(lua_State *l) {
	CInvContext *ctx;
	CInvLibrary *lib;
	cinv_callconv_t cc = CINV_CC_DEFAULT;
	struct LibStruct *st;

	if (lua_gettop(l) < 1 || lua_gettop(l) > 2) {
		lua_pushstring(l, "usage: clibrary.new(libname[, callingconv])");
		lua_error(l);
	}

	if (lua_gettop(l) == 2) {
		const char *cs = tostring(l, 2);
		if (!strcmp("cdecl", cs))
			cc = CINV_CC_CDECL;
		else if (!strcmp("stdcall", cs))
			cc = CINV_CC_STDCALL;
		else if (!strcmp("fastcall", cs))
			cc = CINV_CC_FASTCALL;
		else if (strcmp("default", cs)) {
			lua_pushstring(l, "unknown calling convention");
			lua_error(l);
		}
	}

	ctx = cinv_context_create();
	if (ctx == NULL) {
		lua_pushstring(l, "out of memory");
		lua_error(l);
	}
	
	lib = cinv_library_create(ctx, tostring(l, 1));
	if (lib == NULL) {
		lua_pushstring(l, cinv_context_geterrormsg(ctx));
		cinv_context_delete(ctx);
		lua_error(l);
	}
	
	lua_newtable(l); // push return value

	lua_newtable(l); // metatable for return value
	lua_getglobal(l, "clibrary");
	lua_setfield(l, -2, "__index");
	lua_setmetatable(l, -2);
	
	st = lua_newuserdata(l, sizeof(struct LibStruct));
	st->ctx = ctx;
	st->lib = lib;
	st->cc = cc;

	lua_newtable(l); // metatable for userdata
	lua_getglobal(l, "clibrary");
	lua_getfield(l, -1, "dispose");
	lua_setfield(l, -3, "__gc");
	lua_pop(l, 1); // pop the clibrary global
	lua_setmetatable(l, -2);

	lua_setfield(l, -2, "ud"); // save userdata in return value

	lua_newtable(l); // table for holding tied variables
	lua_setfield(l, -2, "ties");

	return 1;
}
int _clibrary_dispose(lua_State *l) {
	struct LibStruct *st;

	if (lua_gettop(l) != 1) {
		lua_pushstring(l, "usage: clibrary:dispose()");
		lua_error(l);
	}

	// this function can be called from the __gc metatable
	// or by the user with dispose, handle both cases
	if (lua_type(l, 1) == LUA_TUSERDATA)
		st = touserdata(l, 1);
	else {
		lua_getfield(l, 1, "ud");
		st = touserdata(l, -1);
		lua_pop(l, 1);
	}
	if (st->ctx) {
		if (st->lib) {
			cinv_library_delete(st->ctx, st->lib);
			st->lib = NULL;
		}
		cinv_context_delete(st->ctx);
		st->ctx = NULL;
	}
	return 0;
}

// some functions dealing with the type system
int isvoid(lua_State *l, int index) {
	int ret;
	lua_getfield(l, index, "family");
	ret = !strcmp("void", tostring(l, -1));
	lua_pop(l, 1);
	return ret;
}
int isarray(lua_State *l, int index) {
	int ret = 0;
	lua_getfield(l, index, "array");
	if (!lua_isnil(l, -1))
		ret = 1;
	lua_pop(l, 1);
	return ret;
}
int isstruct(lua_State *l, int index) {
	int ret;
	lua_getfield(l, index, "family");
	ret = !strcmp("struct", tostring(l, -1));
	lua_pop(l, 1);
	return ret;
}
int isstring(lua_State *l, int index) {
	int ret;
	lua_getfield(l, index, "family");
	ret = !strcmp("string", tostring(l, -1));
	lua_pop(l, 1);
	return ret;
}
int isbasic(lua_State *l, int index) {
	int ret;
	lua_getfield(l, index, "family");
	ret = !strcmp("basic", tostring(l, -1));
	lua_pop(l, 1);
	return ret;
}
int iscallback(lua_State *l, int index) {
	int ret;
	lua_getfield(l, index, "family");
	ret = !strcmp("callback", tostring(l, -1));
	lua_pop(l, 1);
	return ret;
}

// dont need explicit dispose methods for structures, callbacks or functions
struct StrStruct {
	CInvContext *ctx;
	CInvStructure *st;
};
int _cstructure_gc(lua_State *l) {
	struct StrStruct *st = touserdata(l, 1);
	if (st->ctx) {
		if (st->st) {
			cinv_structure_delete(st->ctx, st->st);
			st->st = NULL;
		}
		cinv_context_delete(st->ctx);
		st->ctx = NULL;
	}
	return 0;
}

int _cstructure_new(lua_State *l) {
	struct StrStruct *ptr;
	CInvContext *ctx;
	CInvStructure *st;
	int numargs, i;
	int size;

	ctx = cinv_context_create();
	if (ctx == NULL) {
		lua_pushstring(l, "out of memory");
		lua_error(l);
	}
	
	st = cinv_structure_create(ctx);
	if (!st) {
		lua_pushstring(l, cinv_context_geterrormsg(ctx));
		cinv_context_delete(ctx);
		lua_error(l);
	}

	numargs = lua_gettop(l);
	if ((numargs % 2) != 0) {
		lua_pushstring(l,
			"usage: cstructure.new(type, name[, type, name ...]");
		goto error;
	}

	for (i = 1; i <= numargs; i += 2) {
		const char *family;

		if (isarray(l, i)) {
			lua_pushstring(l, "array structure members not supported");
			lua_error(l);
		}

		lua_getfield(l, i, "family");
		family = tostring(l, -1);
		if (!strcmp(family, "void")) {
			lua_pushstring(l, "void is not a type");
			goto error;
		} else if (!strcmp(family, "callback")) {
			lua_pushstring(l, "callbacks cannot be struct members");
			goto error;
		} else if (!strcmp(family, "string")) {
			if (!cinv_structure_addmember_value(ctx, st,
				tostring(l, i + 1), CINV_T_PTR)) {
				lua_pushstring(l, cinv_context_geterrormsg(ctx));
				goto error;
			}
		} else if (!strcmp(family, "struct")) {
			struct StrStruct *mem;
			lua_getfield(l, i, "ud");
			mem = touserdata(l, -1);
			if (!cinv_structure_addmember_struct(ctx, st,
				tostring(l, i + 1), mem->st)) {
				lua_pushstring(l, cinv_context_geterrormsg(ctx));
				goto error;
			}
			lua_pop(l, 1);
		} else if (!strcmp(family, "basic")) {
			lua_Integer id;
			lua_getfield(l, i, "id");
			id = tointeger(l, -1);
			if (!cinv_structure_addmember_value(ctx, st,
				tostring(l, i + 1), (cinv_type_t)id)) {
				lua_pushstring(l, cinv_context_geterrormsg(ctx));
				goto error;
			}
			lua_pop(l, 1);
		} else {
			lua_pushstring(l, "unknown family");
			goto error;
		}
		lua_pop(l, 1);
	} 

	if (!cinv_structure_finish(ctx, st)) {
		lua_pushstring(l, cinv_context_geterrormsg(ctx));
		goto error;
	}

	lua_newtable(l); // return value
	// we dont need a metatable for this because there are no methods
	lua_pushstring(l, "struct");
	lua_setfield(l, -2, "family");

	lua_newtable(l); // copy arguments to members table
	for (i = 1; i <= numargs; i += 2) {
		lua_pushvalue(l, i + 1);
		lua_pushvalue(l, i);
		lua_settable(l, -3);
	}
	lua_setfield(l, -2, "members");

	if (!cinv_structure_getsize(ctx, st, &size)) {
		lua_pushstring(l, cinv_context_geterrormsg(ctx));
		goto error;
	}
	lua_pushinteger(l, size);
	lua_setfield(l, -2, "size");

	ptr = lua_newuserdata(l, sizeof(struct StrStruct));
	ptr->ctx = ctx;
	ptr->st = st;
	lua_newtable(l); // metatable for userdata
	lua_pushcfunction(l, _cstructure_gc);
	lua_setfield(l, -2, "__gc");
	lua_setmetatable(l, -2);

	lua_setfield(l, -2, "ud"); // set userdata to field
	
	return 1;

error:
	cinv_structure_delete(ctx, st);
	cinv_context_delete(ctx);
	lua_error(l);
	return 0; // never executed
}

// helper for parsefunction
char getcode(lua_State *l, const char *family, int tblindex, int callback) {
	if (isarray(l, tblindex)) {
		if (callback) {
			lua_pushstring(l, "callbacks cannot accept arrays as arguments");
			return (char)-1;
		}
		return 'p';
	}

	if (!strcmp(family, "struct")) {
		lua_pushstring(l,
			"passing or returning a struct by value is not yet supported");
		return (char)-1;
	} else if (!strcmp(family, "string")) {
		return 'p';
	} else if (!strcmp(family, "callback")) {
		if (callback) {
			lua_pushstring(l, "callbacks cannot accept callbacks as arguments");
			return (char)-1;
		}
		return 'p';
	} else if (!strcmp(family, "basic")) {
		lua_Integer charid;
		lua_getfield(l, tblindex, "charid");
		charid = tointeger(l, -1);
		lua_pop(l, 1);
		return (char)charid;
	} else {
		lua_pushstring(l, "unknown family");
		return (char)-1;
	}
}

CInvFunction *parsefunction(lua_State *l, CInvContext *ctx, int startarg,
	int callback, cinv_callconv_t cc) {
	CInvFunction *ret;
	char retfmt[2];
	char *parmfmt;
	int i;
	int numparms = lua_gettop(l) - startarg - 1;
	const char *family;

	if (lua_isnil(l, startarg)) {
		lua_pushstring(l, "got nil value instead of type");
		lua_error(l);
	}
	lua_getfield(l, startarg, "family");
	if (lua_isnil(l, -1)) {
		lua_pushstring(l, "invalid type");
		lua_error(l);
	}
	lua_pop(l, 1);
	if (isarray(l, startarg)) {
		lua_pushstring(l, "returning arrays not supported");
		lua_error(l);
	}
	if (iscallback(l, startarg)) {
		lua_pushstring(l, "returning callbacks not supported");
		lua_error(l);
	}
	if (callback && isstring(l, startarg)) {
		lua_pushstring(l, "callbacks cannot return strings");
		lua_error(l);
	}

	lua_getfield(l, startarg, "family");
	family = tostring(l, -1);
	if (!strcmp("void", family))
		retfmt[0] = '\0';
	else {
		retfmt[0] = getcode(l, family, startarg, callback);
		if (retfmt[0] == (char)-1)
			lua_error(l);
		retfmt[1] = '\0';
	}
	lua_pop(l, 1);

	parmfmt = malloc(numparms + 1);
	if (!parmfmt) {
		lua_pushstring(l, "out of memory");
		lua_error(l);
	}
	for (i = startarg + 2; i < (numparms + startarg + 2); i++) {
		if (lua_isnil(l, i)) {
			lua_pushstring(l, "got nil value instead of type");
			lua_error(l);
		}
		lua_getfield(l, i, "family");
		if (lua_isnil(l, -1)) {
			lua_pushstring(l, "invalid type");
			lua_error(l);
		}
		family = tostring(l, -1);
		lua_pop(l, 1);
		if (!strcmp("void", family)) {
			// only put up with void if it is the first and only parameter
			if (i == numparms && i == (startarg + 2)) {
				parmfmt[0] = '\0';
			} else {
				free(parmfmt);
				lua_pushstring(l, "void is an invalid parameter type");
				lua_error(l);
			}
		} else {
			parmfmt[i - (startarg + 2)] = getcode(l, family, i, callback);
			if (parmfmt[i - (startarg + 2)] == (char)-1) {
				free(parmfmt);
				lua_error(l);
			}
		}
	}
	parmfmt[i - (startarg + 2)] = '\0';

	ret = cinv_function_create(ctx, cc, retfmt, parmfmt);
	if (!ret) {
		free(parmfmt);
		lua_pushstring(l, cinv_context_geterrormsg(ctx));
		lua_error(l);
	}

	free(parmfmt);
	return ret;
}

int get_arrelement_size(lua_State *l, int type) {
	int ret;
	lua_getfield(l, type, "size");
	ret = (int)tointeger(l, -1);
	lua_pop(l, 1);
	return ret;
}
void push_ptr_val(lua_State *l, void *ptr) {
	lua_pushlightuserdata(l, ptr);
}
void *get_ptr_val(lua_State *l, int index) {
	void *ret;
	const char *pstr;
	if (lua_isnil(l, index))
		return 0;
	if(lua_isuserdata(l, index))
		return lua_touserdata(l, index);
	pstr = lua_tostring(l, index);
	if (strlen(pstr) == 0) return 0;
	if (strlen(pstr) == 1 ||
		strncmp("0x", pstr, 2)) {
		char *endptr;
		ret = (void *)strtol(pstr, &endptr, 16);
		if (*endptr != '\0') {
			lua_pushstring(l, "invalid pointer value");
			lua_error(l);
		}
	} else {
		if (sscanf(pstr, "%p", &ret) == 0) {
			lua_pushstring(l, "invalid pointer value");
			lua_error(l);
		}
	}
	return ret;
}


void *allocreturn(lua_State *l, int typeindex) {
	int sz = get_arrelement_size(l, typeindex);
	void *ret;

	ret = malloc(sz);
	if (!ret) {
		lua_pushstring(l, "out of memory");
		lua_error(l);
	}
	return ret;
}
void marshal_callback(lua_State *l, void *ret, int argindex) {
	if (lua_isnil(l, argindex))
		*(void **)ret = NULL;
	else {
		lua_getfield(l, argindex, "ep");
		*(void **)ret = tolightuserdata(l, -1);
		lua_pop(l, 1);
	}
}
void marshal_string(lua_State *l, void *ret, int argindex) {
	if (lua_isnil(l, argindex))
		*(const char **)ret = NULL;
	else
		*(const char **)ret = tostring(l, argindex);
}
void marshal_basic(lua_State *l, void *ret, int typeindex, int argindex) {
	cinv_type_t id;
	int isnil;

	lua_getfield(l, typeindex, "id");
	id = (cinv_type_t)tointeger(l, -1);
	lua_pop(l, 1);

	isnil = lua_isnil(l, argindex);
	if (isnil && id != CINV_T_PTR) {
		lua_pushstring(l, "cannot convert nil to a number");
		lua_error(l);
	}

	switch (id) {
	case CINV_T_CHAR:
		*(char*)ret = tostring(l, argindex)[0];
		break;
	case CINV_T_SHORT:
		*(short*)ret = (short)tointeger(l, argindex);
		break;
	case CINV_T_INT:
		*(int*)ret = (int)tointeger(l, argindex);
		break;
	case CINV_T_LONG:
		*(long*)ret = (long)tonumber(l, argindex);
		break;
	case CINV_T_EXTRALONG:
		*(long long*)ret = (long long)tonumber(l, argindex);
		break;
	case CINV_T_FLOAT:
		*(float*)ret = (float)tonumber(l, argindex);
		break;
	case CINV_T_DOUBLE:
		*(double*)ret = (double)tonumber(l, argindex);
		break;
	case CINV_T_PTR:
		*(void**)ret = get_ptr_val(l, argindex);
		break;
	default:
		lua_pushstring(l, "unknown type");
		lua_error(l);
	}
}
void marshal_struct(lua_State *l, void *ret, int typeindex, int argindex) {
	struct StrStruct *st;
	int members;
	
	if (lua_isnil(l, argindex)) {
		lua_pushstring(l, "structs cannot be nil");
		lua_error(l);
	}

	lua_getfield(l, typeindex, "ud");
	st = touserdata(l, -1);
	lua_pop(l, 1);

	lua_getfield(l, typeindex, "members");
	members = lua_gettop(l);

	lua_pushnil(l);
	while (lua_next(l, members) != 0) {
		const char *key = tostring(l, -2);
		int type = lua_gettop(l);
		void *val;

		val = malloc(get_arrelement_size(l, type));
		if (!val) {
			lua_pushstring(l, "out of memory");
			lua_error(l);
		}

		lua_getfield(l, argindex, key);
		if (isstruct(l, type))
			marshal_struct(l, val, type, lua_gettop(l));
		if (isstring(l, type))
			marshal_string(l, val, lua_gettop(l));
		else
			marshal_basic(l, val, type, lua_gettop(l));

		if (!cinv_structure_instance_setvalue(st->ctx, st->st,
			ret, key, val)) {
			free(val);
			lua_pushstring(l, cinv_context_geterrormsg(st->ctx));
			lua_error(l);
		}
		free(val);
		lua_pop(l, 2); // pop the value and field
	}

	lua_pop(l, 1);
}
void *marshal_param(lua_State *l, int typeindex, int argindex) {
	void *ret = NULL;
	if (isarray(l, typeindex)) {
		char **ptr = malloc(sizeof(char *));
		if (ptr) {
			if (lua_isnil(l, argindex))
				*ptr = NULL;
			else {
				int arrlen = (int)lua_objlen(l, argindex);
				int elsize = get_arrelement_size(l, typeindex);
				*ptr = malloc(arrlen * elsize);
				if (*ptr) {
					int i;
					char *tmp = *ptr;

					for (i = 1; i <= arrlen; i++) {
						lua_pushinteger(l, i);
						lua_gettable(l, argindex);

						if (lua_isnil(l, -1)) {
							lua_pop(l, 1);
							break;
						}

						if (isstruct(l, typeindex))
							marshal_struct(l, tmp, typeindex, lua_gettop(l));
						else if (isstring(l, typeindex))
							marshal_string(l, tmp, lua_gettop(l));
						else
							marshal_basic(l, tmp, typeindex, lua_gettop(l));
									
						lua_pop(l, 1);
					
						tmp += elsize;
					}

					ret = ptr;
				} else
					free(ptr);
			}
		}
	} else {
		ret = malloc(get_arrelement_size(l, typeindex));
		if (ret) {
			if (isstring(l, typeindex))
				marshal_string(l, ret, argindex);
			else if (iscallback(l, typeindex))
				marshal_callback(l, ret, argindex);
			else
				marshal_basic(l, ret, typeindex, argindex);
		}
	}
	if (!ret) {
		lua_pushstring(l, "out of memory");
		lua_error(l);
	}
	return ret;
}
void free_param(lua_State *l, int typeindex, void *value) {
	if (isarray(l, typeindex)) {
		char **arr = value;
		free(*arr);
		free(arr);
	} else
		free(value);
}
void unmarshal_retval(lua_State *l, int typeindex, void *value) {
	// can be basic or string
	// push completed value on stack
	int famindex;
	lua_getfield(l, typeindex, "family");
	famindex = lua_gettop(l);
	if (!strcmp("basic", tostring(l, famindex))) {
		char strbuf[2];
		int idindex;
		lua_getfield(l, typeindex, "id");
		idindex = lua_gettop(l);
		switch ((cinv_type_t)tointeger(l, idindex)) {
		case CINV_T_CHAR:
			strbuf[0] = *(char *)value;
			strbuf[1] = '\0';
			lua_pushlstring(l, strbuf, 1);
			break;
		case CINV_T_SHORT:
			lua_pushinteger(l, *(short *)value);
			break;
		case CINV_T_INT:
			lua_pushinteger(l, *(int *)value);
			break;
		case CINV_T_LONG:
			lua_pushnumber(l, *(long *)value);
			break;
		case CINV_T_EXTRALONG:
			lua_pushnumber(l, (lua_Number)*(long long *)value);
			break;
		case CINV_T_FLOAT:
			lua_pushnumber(l, *(float *)value);
			break;
		case CINV_T_DOUBLE:
			lua_pushnumber(l, *(double *)value);
			break;
		case CINV_T_PTR:
			push_ptr_val(l, *(void **)value);
			break;
		default:
			lua_pushstring(l, "unknown type");
			lua_error(l);
			break;
		}
		lua_remove(l, idindex);
	} else
		lua_pushstring(l, *(char **)value);
	lua_remove(l, famindex);
}
void unmarshal_struct(lua_State *l, int typeindex, void *instance) {
	struct StrStruct *st;
	int members, ret;

	lua_newtable(l);
	ret = lua_gettop(l);

	lua_getfield(l, typeindex, "ud");
	st = touserdata(l, -1);
	lua_pop(l, 1);

	lua_getfield(l, typeindex, "members");
	members = lua_gettop(l);

	lua_pushnil(l);
	while (lua_next(l, members) != 0) {
		const char *key = tostring(l, -2);
		int type = lua_gettop(l);
		
		void *val = cinv_structure_instance_getvalue(st->ctx, st->st,
			instance, key);
		if (val == NULL) {
			lua_pushstring(l, cinv_context_geterrormsg(st->ctx));
			lua_error(l);
		}

		if (isstruct(l, type))
			unmarshal_struct(l, type, val);
		else
			unmarshal_retval(l, type, val);

		lua_setfield(l, ret, key);

		lua_pop(l, 1); // pop the value
	}

	lua_remove(l, members);
}
void unmarshal_array(lua_State *l, int typeindex, void *value, int outindex) {
	char *cptr = *(char **)value;
	int arrlen = (int)lua_objlen(l, outindex), i;
	int elsize = get_arrelement_size(l, typeindex);

	for (i = 1; i <= arrlen; i++) {
		lua_pushinteger(l, i);

		if (isstruct(l, typeindex))
			unmarshal_struct(l, typeindex, cptr);
		else
			unmarshal_retval(l, typeindex, cptr);

		lua_settable(l, outindex);
		cptr += elsize;
	}
}

struct CBStruct {
	CInvCallback *cb;
	CInvFunction *func;
	CInvContext *ctx;
	lua_Integer key;
	lua_State *l;
};

int _ccallback_gc(lua_State *l) {
	struct CBStruct *cb = touserdata(l, 1);

	lua_pushinteger(l, cb->key);
	lua_pushnil(l);
	lua_settable(l, LUA_GLOBALSINDEX);

	if (cb->cb) {
		cinv_callback_delete(cb->ctx, cb->cb);
		cb->cb = NULL;
	}
	if (cb->func) {
		cinv_function_delete(cb->ctx, cb->func);
		cb->func = NULL;
	}

	return 0;
}

void _ccallback_invoked(CInvFunction *f, void *parameters[],
	void *returnout, void *userdata) {
	struct CBStruct *cb = userdata;
	int usertable, pindex, index, retindex;
	int numargs, i;

	lua_pushinteger(cb->l, cb->key);
	lua_gettable(cb->l, LUA_GLOBALSINDEX);
	usertable = lua_gettop(cb->l);
	
	if (lua_isnil(cb->l, usertable)) {
		lua_pushstring(cb->l,
			"C callback being called for an object which has been collected");
		lua_error(cb->l);
	}
	lua_getfield(cb->l, usertable, "cbfunc");

	lua_getfield(cb->l, usertable, "params");
	pindex = lua_gettop(cb->l);
	numargs = (int)lua_objlen(cb->l, pindex);

	for (i = 0; i < numargs; i++) {
		lua_pushinteger(cb->l, i + 1);
		lua_gettable(cb->l, pindex);
		index = lua_gettop(cb->l);

		unmarshal_retval(cb->l, index, parameters[i]);

		lua_remove(cb->l, index);
	}
	lua_remove(cb->l, pindex);

	lua_call(cb->l, numargs, 1);
	retindex = lua_gettop(cb->l);

	lua_getfield(cb->l, usertable, "return");
	index = lua_gettop(cb->l);
	if (!isvoid(cb->l, index)) {
		marshal_basic(cb->l, returnout, index, retindex);
	}
	lua_remove(cb->l, index);

	lua_pop(cb->l, 2); // return value and usertable
}

int _clibrary_new_callback(lua_State *l) {
	struct CBStruct *cbs;
	struct LibStruct *lib;
	CInvFunction *func;
	CInvCallback *cb;
	int i;
	void *ep;
	int retval;
	int numargs = lua_gettop(l);
	if (numargs < 3) {
		lua_pushstring(l, "usage: clibrary:new_callback(rettype, cbfunc, ...)");
		lua_error(l);
	}

	lua_getfield(l, 1, "ud");
	lib = touserdata(l, -1);
	lua_pop(l, 1);

	func = parsefunction(l, lib->ctx, 2, 1, lib->cc);

	lua_newtable(l);
	retval = lua_gettop(l);

	cbs = lua_newuserdata(l, sizeof(struct CBStruct));
	cbs->func = func;
	cbs->ctx = lib->ctx;
	cbs->l = l;
	
	cb = cinv_callback_create(lib->ctx, func, cbs, _ccallback_invoked);
	if (!cb) {
		lua_pushstring(l, cinv_context_geterrormsg(lib->ctx));
		cinv_function_delete(lib->ctx, func);
		lua_error(l);
	}
	cbs->cb = cb;
	cbs->key = time(NULL);

	while (1) {
		lua_pushinteger(l, cbs->key);
		lua_gettable(l, LUA_GLOBALSINDEX);
		if (!lua_isnil(l, -1)) {
			lua_pop(l, 1);
			cbs->key++;
			continue;
		}
		lua_pop(l, 1);
		lua_pushinteger(l, cbs->key);
		lua_pushvalue(l, retval);
		lua_settable(l, LUA_GLOBALSINDEX);
		break;
	}

	lua_newtable(l);
	lua_pushcfunction(l, _ccallback_gc);
	lua_setfield(l, -2, "__gc");
	lua_setmetatable(l, -2);
	lua_setfield(l, -2, "ud");

	ep = cinv_callback_getentrypoint(lib->ctx, cb);
	if (!ep) {
		lua_pushstring(l, cinv_context_geterrormsg(lib->ctx));
		lua_error(l);
	}
	lua_pushlightuserdata(l, ep);
	lua_setfield(l, -2, "ep");

	lua_pushvalue(l, 2);
	lua_setfield(l, -2, "return");
	
	lua_newtable(l);
	for (i = 4; i <= numargs; i++) {
		lua_pushinteger(l, i - 3);
		lua_pushvalue(l, i);
		if (isvoid(l, lua_gettop(l)))
			lua_pop(l, 2);
		else
			lua_settable(l, -3);
	}
	lua_setfield(l, -2, "params");

	lua_pushvalue(l, 3);
	lua_setfield(l, -2, "cbfunc");

	lua_pushvalue(l, 1);
	lua_setfield(l, -2, "lib");

	return 1;
}

int _cinv_ptr_to_callback(lua_State *l) {
	void *p;
	if (lua_gettop(l) != 1) {
		lua_pushstring(l, "usage: cinv.ptr_to_callback(cptr)");
		lua_error(l);
	}

	p = get_ptr_val(l, 1);

	lua_newtable(l);
	lua_pushlightuserdata(l, p);
	lua_setfield(l, -2, "ep");

	return 1;
}
int _cinv_callback_to_ptr(lua_State *l) {
	void *p;
	if (lua_gettop(l) != 1) {
		lua_pushstring(l, "usage: cinv.callback_to_ptr(callback)");
		lua_error(l);
	}

	lua_getfield(l, 1, "ep");
	p = tolightuserdata(l, -1);
	lua_pop(l, 1);

	push_ptr_val(l, p);

	return 1;
}

struct FunStruct {
	void *ep;
	CInvFunction *func;
	CInvContext *ctx;
};

int _function_gc(lua_State *l) {
	struct FunStruct *fs = touserdata(l, 1);
	if (fs->func) {
		cinv_function_delete(fs->ctx, fs->func);
		fs->func = NULL;
	}
	return 0;
}

int _function_call(lua_State *l) {
	struct FunStruct *fs;
	void *returnval = NULL;
	void **parameters = NULL;
	int numargs = 0;
	int numreturn = 0;
	int index, pindex, i;

	lua_getfield(l, 1, "ud");
	fs = touserdata(l, -1);
	lua_pop(l, 1);

	lua_getfield(l, 1, "return");
	index = lua_gettop(l);
	if (!isvoid(l, index))
		returnval = allocreturn(l, index);
	lua_remove(l, index);

	lua_getfield(l, 1, "params");
	pindex = lua_gettop(l);
	numargs = (int)lua_objlen(l, pindex);
	if (numargs > 0) {
		parameters = malloc(numargs * sizeof(void*));
		for (i = 0; i < numargs; i++) {
			lua_pushinteger(l, i + 1);
			lua_gettable(l, pindex);

			index = lua_gettop(l);
			
			parameters[i] = marshal_param(l, index, i + 2);

			lua_remove(l, index);
		}
	}
	lua_remove(l, pindex);

	cinv_function_invoke(fs->ctx, fs->func, fs->ep,
		returnval, parameters);
	
	lua_getfield(l, 1, "params");
	pindex = lua_gettop(l);
	for (i = 0; i < numargs; i++) {
		lua_pushinteger(l, i + 1);
		lua_gettable(l, pindex);

		index = lua_gettop(l);
		lua_getfield(l, index, "array");
		if (!lua_isnil(l, -1))
			unmarshal_array(l, index, parameters[i], i + 2);
		lua_pop(l, 1);

		free_param(l, index, parameters[i]);

		lua_remove(l, index);
	}
	lua_remove(l, pindex);

	if (returnval != NULL) {
		lua_getfield(l, 1, "return");
		index = lua_gettop(l);
		unmarshal_retval(l, index, returnval);
		numreturn++;
		lua_remove(l, index);
	}

	free(parameters);
	free(returnval);

	return numreturn;
}

int _clibrary_get_function(lua_State *l) {
	struct FunStruct *fs;
	struct LibStruct *lib;
	CInvFunction *func;
	void *ep;
	int i;
	int numargs = lua_gettop(l);
	if (numargs < 3) {
		lua_pushstring(l, "usage: clibrary:get_function(rettype, name, ...)");
		lua_error(l);
	}

	lua_getfield(l, 1, "ud");
	lib = touserdata(l, -1);
	lua_pop(l, 1);

	func = parsefunction(l, lib->ctx, 2, 0, lib->cc);

	ep = cinv_library_load_entrypoint(lib->ctx, lib->lib, tostring(l, 3));
	if (!ep) {
		lua_pushstring(l, cinv_context_geterrormsg(lib->ctx));
		cinv_function_delete(lib->ctx, func);
		lua_error(l);
	}
	
	lua_newtable(l);

	lua_newtable(l);
	lua_pushcfunction(l, _function_call);
	lua_setfield(l, -2, "__call");
	lua_setmetatable(l, -2);

	fs = lua_newuserdata(l, sizeof(struct FunStruct));
	fs->ep = ep;
	fs->func = func;
	fs->ctx = lib->ctx;
	lua_newtable(l);
	lua_pushcfunction(l, _function_gc);
	lua_setfield(l, -2, "__gc");
	lua_setmetatable(l, -2);
	lua_setfield(l, -2, "ud");

	lua_pushvalue(l, 2);
	lua_setfield(l, -2, "return");
	
	lua_newtable(l);
	for (i = 4; i <= numargs; i++) {
		lua_pushinteger(l, i - 3);
		lua_pushvalue(l, i);
		if (isvoid(l, lua_gettop(l)))
			lua_pop(l, 2);
		else
			lua_settable(l, -3);
	}
	lua_setfield(l, -2, "params");

	// maintain a reference to parent library
	lua_pushvalue(l, 1);
	lua_setfield(l, -2, "lib");

	return 1;
}

int _clibrary_tie_init(lua_State *l) {
	struct LibStruct *lib;
	void *ep;

	if (lua_gettop(l) != 3) {
		lua_pushstring(l, "usage: clibrary:tie_init(type, name)");
		lua_error(l);
	}

	lua_getfield(l, 1, "ud");
	lib = touserdata(l, -1);
	lua_pop(l, 1);

	if (isvoid(l, 2)) {
		lua_pushstring(l, "void is not a type");
		lua_error(l);
	}
	if (isarray(l, 2)) {
		lua_pushstring(l, "tied variables cannot be arrays");
		lua_error(l);
	}
	if (!isbasic(l, 2)) {
		lua_pushstring(l,
			"tied variables cannot be strings, structs or callbacks");
		lua_error(l);
	}

	lua_getfield(l, 1, "ties");
	lua_getfield(l, -1, tostring(l, 3));
	if (!lua_isnil(l, -1)) {
		lua_pushstring(l, "a tied variable with that name already exists");
		lua_error(l);
	}
	lua_pop(l, 1);

	ep = cinv_library_load_entrypoint(lib->ctx, lib->lib, tostring(l, 3));
	if (!ep) {
		lua_pushstring(l, cinv_context_geterrormsg(lib->ctx));
		lua_error(l);
	}
	
	lua_newtable(l);
	lua_pushvalue(l, 2);
	lua_setfield(l, -2, "type");
	lua_pushlightuserdata(l, ep);
	lua_setfield(l, -2, "ep");

	lua_setfield(l, -2, lua_tostring(l, 3));
	lua_pop(l, 1);

	return 0;
}
int _clibrary_tie_get(lua_State *l) {
	void *ep;

	if (lua_gettop(l) != 2) {
		lua_pushstring(l, "usage: clibrary:tie_get(name)");
		lua_error(l);
	}

	lua_getfield(l, 1, "ties");
	lua_pushvalue(l, 2);
	lua_gettable(l, -2);
	if (lua_isnil(l, -1)) {
		lua_pushstring(l, "a tied variable with that name was not found");
		lua_error(l);
	}
	lua_getfield(l, -1, "ep");
	ep = tolightuserdata(l, -1);
	lua_pop(l, 1);

	lua_getfield(l, -1, "type");
	unmarshal_retval(l, lua_gettop(l), ep);
	lua_remove(l, -2);
	lua_remove(l, -2);
	lua_remove(l, -2);
	
	return 1;
}
int _clibrary_tie_set(lua_State *l) {
	void *ep;

	if (lua_gettop(l) != 3) {
		lua_pushstring(l, "usage: clibrary:tie_set(name, value)");
		lua_error(l);
	}

	lua_getfield(l, 1, "ties");
	lua_pushvalue(l, 2);
	lua_gettable(l, -2);
	if (lua_isnil(l, -1)) {
		lua_pushstring(l, "a tied variable with that name was not found");
		lua_error(l);
	}
	lua_getfield(l, -1, "ep");
	ep = tolightuserdata(l, -1);
	lua_pop(l, 1);

	lua_getfield(l, -1, "type");
	marshal_basic(l, ep, lua_gettop(l), 3);
	lua_pop(l, 3);
	
	return 0;
}

void declbasic(lua_State *l, const char *g,
	cinv_type_t id, char charid, int size) {
	lua_newtable(l);
	lua_pushstring(l, "basic");
	lua_setfield(l, -2, "family");
	lua_pushinteger(l, (lua_Integer)id);
	lua_setfield(l, -2, "id");
	lua_pushinteger(l, (lua_Integer)charid);
	lua_setfield(l, -2, "charid");
	lua_pushinteger(l, (lua_Integer)size);
	lua_setfield(l, -2, "size");
	lua_setglobal(l, g);
}

void declfunc(lua_State *l, const char *name, lua_CFunction func) {
	lua_pushcfunction(l, func);
	lua_setfield(l, -2, name);
}

int _cinv_string_to_chararray(lua_State *l) {
	size_t len, i, end;
	int includenil = 1;
	const char *s;
	if (lua_gettop(l) < 1 || lua_gettop(l) > 2) {
		lua_pushstring(l,
			"usage: cinv.string_to_chararray(string[, includenil])");
		lua_error(l);
	}
	if (lua_gettop(l) == 2)
		includenil = toboolean(l, 2);

	s = tolstring(l, 1, &len);
	
	lua_newtable(l);
	end = len + 1;
	if (!includenil)
		end = len;
	for (i = 0; i < len; i++) {
		lua_pushinteger(l, i + 1);
		lua_pushlstring(l, s + i, 1);
		lua_settable(l, -3);
	}

	return 1;
}

int _cinv_chararray_to_string(lua_State *l) {
	size_t len, i;
	if (lua_gettop(l) < 1 || lua_gettop(l) > 2) {
		lua_pushstring(l, "usage: cinv.chararray_to_string(carray[, len])");
		lua_error(l);
	}
	if (lua_gettop(l) == 2) {
		len = lua_tointeger(l, 2);
		if (len < 0) {
			lua_pushstring(l, "invalid length");
			lua_error(l);
		}
	} else {
		len = lua_objlen(l, 1);
	}
	if (len == 0) {
		lua_pushlstring(l, "", len);
	} else {
		char *ret = malloc(len + 1);
		if (!ret) {
			lua_pushstring(l, "out of memory");
			lua_error(l);
		}
		for (i = 0; i < len; i++) {
			lua_pushinteger(l, i + 1);
			lua_gettable(l, 1);

			if (lua_isnil(l, -1)) {
				lua_pop(l, 1);
				break;
			}
			ret[i] = tostring(l, -1)[0];
			lua_pop(l, 1);
		}

		ret[len] = '\0';
		lua_pushlstring(l, ret, len);
		free(ret);
	}
	
	return 1;
}

int _cinv_ptr_to_string(lua_State *l) {
	char *ptr;
	int len = -1;
	if (lua_gettop(l) != 1 && lua_gettop(l) != 2) {
		lua_pushstring(l, "usage: cinv.ptr_to_string(cptr[, len])");
		lua_error(l);
	}
	ptr = get_ptr_val(l, 1);
	if (!lua_gettop(l) == 2) {
		len = (int)tointeger(l, 2);
		if (len < 0) {
			lua_pushstring(l, "invalid length parameter");
			lua_error(l);
		}
	}
	if (ptr == NULL)
		lua_pushnil(l);
	else if (len != -1)
		lua_pushlstring(l, ptr, len);
	else
		lua_pushstring(l, ptr);
	return 1;
}

int _cinv_ptr_to_array(lua_State *l) {
	int len, i, index;
	void *value;
	if (lua_gettop(l) != 3) {
		lua_pushstring(l, "usage: cinv.ptr_to_array(cptr, arrtype, len)");
		lua_error(l);
	}
	
	if (!isstruct(l, 2) &&
		!isstring(l, 2) &&
		!isbasic(l, 2)) {
		lua_pushstring(l, "type is not a valid array type");
		lua_error(l);
	}
	
	value = get_ptr_val(l,  1);
	if (value == NULL)
		lua_pushnil(l);
	else {
		len = (int)tointeger(l, 3);
		// create table of desired len and call unmarshal_array
		lua_newtable(l);
		index = lua_gettop(l);
		for (i = 1; i <= len; i++) {
			lua_pushinteger(l, i);
			lua_pushstring(l, "");
			lua_settable(l, index);
		}
		unmarshal_array(l, 2, &value, index);
	}
	return 1;
}

int _cinv_ptr_to_struct(lua_State *l) {
	void *ptr;
	if (lua_gettop(l) != 2) {
		lua_pushstring(l, "usage: cinv.ptr_to_struct(cptr, type)");
		lua_error(l);
	}
	ptr = get_ptr_val(l, 1);
	if (ptr == NULL)
		lua_pushnil(l);
	else
		unmarshal_struct(l, 2, ptr);
	return 1;
}

int _cinv_sizeof(lua_State *l) {
	if (lua_gettop(l) != 1) {
		lua_pushstring(l, "usage: cinv.sizeof(type)");
		lua_error(l);
	}
	if (isvoid(l, 1)) {
		lua_pushstring(l, "void is not a type");
		lua_error(l);
	}
	if (isarray(l, 1)) {
		lua_pushstring(l, "cannot get size of array types");
		lua_error(l);
	}
	
	lua_pushinteger(l, get_arrelement_size(l, 1));
	return 1;
}
int _cinv_ptr_incr(lua_State *l) {
	char *ptr;
	int num;
	if (lua_gettop(l) != 2) {
		lua_pushstring(l, "usage: cinv.ptr_incr(cptr, numbytes)");
		lua_error(l);
	}

	num = (int)tointeger(l, 2);
	ptr = get_ptr_val(l, 1);

	ptr += num;
	push_ptr_val(l, ptr);

	return 1;
}

int _cinv_array(lua_State *l) {
	if (lua_gettop(l) != 1) {
		lua_pushstring(l, "usage: cinv.array(arrtype)");
		lua_error(l);
	}
	
	if (isarray(l, 1)) {
		lua_pushstring(l, "arrays of arrays not supported");
		lua_error(l);
	}
	if (isvoid(l, 1)) {
		lua_pushstring(l, "void is not a type");
		lua_error(l);
	}
	if (iscallback(l, 1)) {
		lua_pushstring(l, "arrays of callbacks not supported");
		lua_error(l);
	}
	
	lua_newtable(l);
	lua_pushnil(l);
	while (lua_next(l, 1) != 0) {
		lua_pushvalue(l, -2);
		lua_pushvalue(l, -2);
		lua_settable(l, -5);
		lua_pop(l, 1);
	}

	lua_pushstring(l, "yes");
	lua_setfield(l, -2, "array");

	return 1;
}

DLLEXPORT int luaopen_cinvoke_lua(lua_State *l) {
	declbasic(l, "Cchar", CINV_T_CHAR, 'c', 1);
	declbasic(l, "Cshort", CINV_T_SHORT, 's', sizeof(short));
	declbasic(l, "Cint", CINV_T_INT, 'i', sizeof(int));
	declbasic(l, "Clong", CINV_T_LONG, 'l', sizeof(long));
	declbasic(l, "Clonglong", CINV_T_EXTRALONG, 'e', sizeof(long long));
	declbasic(l, "Cfloat", CINV_T_FLOAT, 'f', sizeof(float));
	declbasic(l, "Cdouble", CINV_T_DOUBLE, 'd', sizeof(double));
	declbasic(l, "Cptr", CINV_T_PTR, 'p', sizeof(void*));
	declbasic(l, "Cint16", CINV_T_2BYTE, '2', 2);
	declbasic(l, "Cint32", CINV_T_4BYTE, '4', 4);
	declbasic(l, "Cint64", CINV_T_8BYTE, '8', 8);

	lua_newtable(l);
	lua_pushstring(l, "string");
	lua_setfield(l, -2, "family");
	lua_pushinteger(l, sizeof(char*));
	lua_setfield(l, -2, "size");
	lua_setglobal(l, "Cstring");

	lua_newtable(l);
	lua_pushstring(l, "callback");
	lua_setfield(l, -2, "family");
	lua_pushinteger(l, sizeof(void*));
	lua_setfield(l, -2, "size");
	lua_setglobal(l, "Ccallback");

	lua_newtable(l);
	lua_pushstring(l, "void");
	lua_setfield(l, -2, "family");
	lua_setglobal(l, "Cvoid");

	lua_newtable(l);
	declfunc(l, "array", _cinv_array);
	declfunc(l, "chararray_to_string", _cinv_chararray_to_string);
	declfunc(l, "string_to_chararray", _cinv_string_to_chararray);
	declfunc(l, "ptr_to_string", _cinv_ptr_to_string);
	declfunc(l, "ptr_to_array", _cinv_ptr_to_array);
	declfunc(l, "ptr_to_struct", _cinv_ptr_to_struct);
	declfunc(l, "ptr_to_callback", _cinv_ptr_to_callback);
	declfunc(l, "callback_to_ptr", _cinv_callback_to_ptr);
	declfunc(l, "sizeof", _cinv_sizeof);
	declfunc(l, "ptr_incr", _cinv_ptr_incr);
	lua_setglobal(l, "cinv");

	lua_newtable(l);
	declfunc(l, "new", _clibrary_new);
	declfunc(l, "get_function", _clibrary_get_function);
	declfunc(l, "new_callback", _clibrary_new_callback);
	declfunc(l, "dispose", _clibrary_dispose);
	declfunc(l, "tie_init", _clibrary_tie_init);
	declfunc(l, "tie_get", _clibrary_tie_get);
	declfunc(l, "tie_set", _clibrary_tie_set);
	lua_setglobal(l, "clibrary");

	lua_newtable(l);
	declfunc(l, "new", _cstructure_new);
	lua_setglobal(l, "cstructure");
	
	return 0;
}
