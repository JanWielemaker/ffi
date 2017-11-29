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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <cinvoke.h>
#include "org_cinvoke_Natives.h"

#define T_JSHORT -1
#define T_JINT -2
#define T_JLONG -3

#if __APPLE_CC__ || __sun__
#define TO_PTR(jl) ((void *)(int)(jl))
#define TO_LONG(p) ((jlong)(int)(p))
#else
#define TO_PTR(jl) ((void *)(jl))
#define TO_LONG(p) ((jlong)(p))
#endif

JNIEXPORT jlong JNICALL Java_org_cinvoke_Natives_createContext(
	JNIEnv *env, jclass) {
	return TO_LONG(cinv_context_create());
}
JNIEXPORT jstring JNICALL Java_org_cinvoke_Natives_getError(
	JNIEnv *env, jclass, jlong c) {
	CInvContext *ctx = (CInvContext *)TO_PTR(c);
	return env->NewStringUTF(cinv_context_geterrormsg(ctx));
}
JNIEXPORT jint JNICALL Java_org_cinvoke_Natives_deleteContext(
	JNIEnv *env, jclass, jlong c) {
	CInvContext *ctx = (CInvContext *)TO_PTR(c);
	return cinv_context_delete(ctx);
}
JNIEXPORT jlong JNICALL Java_org_cinvoke_Natives_createLibrary(
	JNIEnv *env, jclass, jlong c, jstring libname) {
	CInvContext *ctx = (CInvContext *)TO_PTR(c);
	const char *chrs = env->GetStringUTFChars(libname, NULL);
	if (chrs == NULL) return 0;

	jlong ret = TO_LONG(cinv_library_create(ctx, chrs));

	env->ReleaseStringUTFChars(libname, chrs);
	return ret;
}
JNIEXPORT jlong JNICALL Java_org_cinvoke_Natives_loadEPLibrary(
	JNIEnv *env, jclass, jlong c, jlong l, jstring name) {
	CInvContext *ctx = (CInvContext *)TO_PTR(c);
	CInvLibrary *lib = (CInvLibrary *)TO_PTR(l);
	const char *chrs = env->GetStringUTFChars(name, NULL);
	if (chrs == NULL) return 0;

	jlong ret = TO_LONG(cinv_library_load_entrypoint(ctx, lib, chrs));

	env->ReleaseStringUTFChars(name, chrs);
	return ret;
}
JNIEXPORT jint JNICALL Java_org_cinvoke_Natives_deleteLibrary(
	JNIEnv *env, jclass, jlong c, jlong l) {
	CInvContext *ctx = (CInvContext *)TO_PTR(c);
	CInvLibrary *lib = (CInvLibrary *)TO_PTR(l);
	
	return cinv_library_delete(ctx, lib);
}
JNIEXPORT jlong JNICALL Java_org_cinvoke_Natives_createFunction(
	JNIEnv *env, jclass, jlong c, jint cc, jstring retfmt, jstring parmfmt) {
	CInvContext *ctx = (CInvContext *)TO_PTR(c);
	const char *parmchrs = env->GetStringUTFChars(parmfmt, NULL);
	if (parmchrs == NULL) return 0;
	const char *retchrs = env->GetStringUTFChars(retfmt, NULL);
	if (retchrs == NULL) return 0;

	if (cc == -1)
		cc = CINV_CC_DEFAULT;

	jlong ret = TO_LONG(cinv_function_create(ctx, (cinv_callconv_t)cc,
		retchrs, parmchrs));

	env->ReleaseStringUTFChars(parmfmt, parmchrs);
	env->ReleaseStringUTFChars(retfmt, retchrs);
	return ret;
}
void *alloc(int type) {
	return malloc(Java_org_cinvoke_Natives_sizeofBasic(NULL, NULL, type));
}
void fail(JNIEnv *env) {
	jclass ex = env->FindClass("java/lang/Exception");
	env->ThrowNew(ex, "invoke failed");
}
JNIEXPORT jobject JNICALL Java_org_cinvoke_Natives_invokeFunction(
	JNIEnv *env, jclass, jlong c, jlong f, jlong e, jobjectArray params,
	jintArray types, jclass retcls, jint rettype) {
	CInvContext *ctx = (CInvContext *)TO_PTR(c);
	CInvFunction *func = (CInvFunction *)TO_PTR(f);
	void *ep = (void *)TO_PTR(e);
	jobject ret = NULL;
	void *retp = NULL;
	void **pp = NULL;
	jint *tarr = NULL;
	int numparms = 0, np = env->GetArrayLength(params);
	if (env->ExceptionOccurred()) {
		fail(env);
		goto out;
	}
	tarr = env->GetIntArrayElements(types, NULL);
	if (tarr == NULL) {
		fail(env);
		goto out;
	}
	
	if (retcls) {
		retp = alloc(rettype);
		if (!retp) {
			fail(env);
			goto out;
		}
	}
	pp = (void **)malloc(sizeof(void*) * np);
	if (!pp) {
		fail(env);
		goto out;
	}
	for (int i = 0; i < np; i++)
		pp[i] = NULL;
	for (int i = 0; i < np; i++) {
		jobject parm = env->GetObjectArrayElement(params, i);
		if (env->ExceptionOccurred()) {
			fail(env);
			goto out;
		}
		pp[i] = alloc(tarr[i]);
		if (!pp[i]) {
			fail(env);
			goto out;
		}
		Java_org_cinvoke_Natives_writeValue(env, NULL, TO_LONG(pp[i]), parm,
			tarr[i]);
	}
	numparms = np;
	
	if (!cinv_function_invoke(ctx, func, ep, retp, pp)) {
		fail(env);
		goto out;
	}
	
	if (retp)
		ret = Java_org_cinvoke_Natives_readValue(env, NULL,
			TO_LONG(retp), retcls, rettype);
out:
	if (tarr != NULL)
		env->ReleaseIntArrayElements(types, tarr, 0);
	for (int i = 0; i < numparms; i++)
		free(pp[i]);
	free(pp);
	free(retp);
	return ret;
}
JNIEXPORT jint JNICALL Java_org_cinvoke_Natives_deleteFunction(
	JNIEnv *env, jclass, jlong c, jlong f) {
	CInvContext *ctx = (CInvContext *)TO_PTR(c);
	CInvFunction *func = (CInvFunction *)TO_PTR(f);

	return cinv_function_delete(ctx, func);
}
JNIEXPORT jlong JNICALL Java_org_cinvoke_Natives_createStruct(
	JNIEnv *env, jclass, jlong c) {
	CInvContext *ctx = (CInvContext *)TO_PTR(c);
	
	return TO_LONG(cinv_structure_create(ctx));
}
JNIEXPORT jint JNICALL Java_org_cinvoke_Natives_addValueMemberStruct(
	JNIEnv *env, jclass, jlong c, jlong s, jstring name, jint type) {
	CInvContext *ctx = (CInvContext *)TO_PTR(c);
	CInvStructure *st = (CInvStructure *)TO_PTR(s);
	const char *chrs = env->GetStringUTFChars(name, NULL);
	if (chrs == NULL) return 0;

	jint ret = cinv_structure_addmember_value(ctx, st, chrs, (cinv_type_t)type);

	env->ReleaseStringUTFChars(name, chrs);
	return ret;
}
JNIEXPORT jint JNICALL Java_org_cinvoke_Natives_addStructMemberStruct(
	JNIEnv *env, jclass, jlong c, jlong s, jstring name, jlong t) {
	CInvContext *ctx = (CInvContext *)TO_PTR(c);
	CInvStructure *st = (CInvStructure *)TO_PTR(s);
	CInvStructure *type = (CInvStructure *)TO_PTR(t);
	const char *chrs = env->GetStringUTFChars(name, NULL);
	if (chrs == NULL) return 0;

	jint ret = cinv_structure_addmember_struct(ctx, st, chrs, type);

	env->ReleaseStringUTFChars(name, chrs);
	return ret;
}
JNIEXPORT jlong JNICALL Java_org_cinvoke_Natives_alloc(
	JNIEnv *env, jclass, jint sz) {
	return TO_LONG(malloc(sz));
}
JNIEXPORT void JNICALL Java_org_cinvoke_Natives_free(
	JNIEnv *env, jclass, jlong p) {
	void *ptr = (void *)TO_PTR(p);
	free(ptr);
}
JNIEXPORT jlong JNICALL Java_org_cinvoke_Natives_writeValue(
	JNIEnv *env, jclass, jlong p, jobject val, jint type) {
	jclass cls = env->GetObjectClass(val);
	if (!cls) return 0;

	jmethodID meth;
	jbyte b; jshort s; jint i; jlong l; jfloat f; jdouble d;

	switch (type) {
	case CINV_T_CHAR:
		meth = env->GetMethodID(cls, "byteValue", "()B");
		if (!meth) return 0;
		b = env->CallByteMethod(val, meth);
		break;
	case CINV_T_SHORT:
	case T_JINT:
		meth = env->GetMethodID(cls, "intValue", "()I");
		if (!meth) return 0;
		i = env->CallIntMethod(val, meth);
		break;
	case CINV_T_INT:
	case CINV_T_LONG:
	case CINV_T_EXTRALONG:
	case CINV_T_PTR:
	case T_JLONG:
		meth = env->GetMethodID(cls, "longValue", "()J");
		if (!meth) return 0;
		l = env->CallLongMethod(val, meth);
		break;
	case CINV_T_FLOAT:
		meth = env->GetMethodID(cls, "floatValue", "()F");
		if (!meth) return 0;
		f = env->CallFloatMethod(val, meth);
		break;
	case CINV_T_DOUBLE:
		meth = env->GetMethodID(cls, "doubleValue", "()D");
		if (!meth) return 0;
		d = env->CallDoubleMethod(val, meth);
		break;
	case T_JSHORT:
		meth = env->GetMethodID(cls, "shortValue", "()S");
		if (!meth) return 0;
		s = env->CallShortMethod(val, meth);
		break;
	}

	if (env->ExceptionOccurred()) return 0;

	switch (type) {
	case CINV_T_CHAR:
		*(char *)TO_PTR(p) = (char)b;
		break;
	case CINV_T_SHORT:
		*(short *)TO_PTR(p) = (short)i;
		break;
	case CINV_T_INT:
		*(int *)TO_PTR(p) = (int)l;
		break;
	case CINV_T_LONG:
		*(long *)TO_PTR(p) = (long)l;
		break;
	case CINV_T_EXTRALONG:
		*(long long *)TO_PTR(p) = (long long)l;
		break;
	case CINV_T_PTR: 
		*(void* *)TO_PTR(p) = (void*)TO_PTR(l);
		break;
	case CINV_T_FLOAT:
		*(float *)TO_PTR(p) = (float)f;
		break;
	case CINV_T_DOUBLE:
		*(double *)TO_PTR(p) = (double)d;
		break;
	case T_JSHORT:
		*(cinv_int16_t *)TO_PTR(p) = (cinv_int16_t)s;
		break;
	case T_JINT:
		*(cinv_int32_t *)TO_PTR(p) = (cinv_int32_t)i;
		break;
	case T_JLONG:
		*(cinv_int64_t *)TO_PTR(p) = (cinv_int64_t)l;
		break;
	}

	char *ptr = (char *)TO_PTR(p);
	ptr += Java_org_cinvoke_Natives_sizeofBasic(env, NULL,  type);
	return TO_LONG(ptr);
}
JNIEXPORT jobject JNICALL Java_org_cinvoke_Natives_readValue(
	JNIEnv *env, jclass, jlong p, jclass cls, jint type) {
	jbyte b; jshort s; jint i; jlong l; jfloat f; jdouble d;
	switch (type) {
	case CINV_T_CHAR:
		b = (jbyte)*(char *)TO_PTR(p);
		break;
	case CINV_T_SHORT:
		i = (jint)*(short *)TO_PTR(p);
		break;
	case CINV_T_INT:
		l = (jlong)*(int *)TO_PTR(p);
		break;
	case CINV_T_LONG:
		l = (jlong)*(long *)TO_PTR(p);
		break;
	case CINV_T_EXTRALONG:
		l = (jlong)*(long long *)TO_PTR(p);
		break;
	case CINV_T_PTR: 
		l = TO_LONG(*(void* *)TO_PTR(p));
		break;
	case CINV_T_FLOAT:
		f = (jfloat)*(float *)TO_PTR(p);
		break;
	case CINV_T_DOUBLE:
		d = (jdouble)*(double *)TO_PTR(p);
		break;
	case T_JSHORT:
		s = (jshort)*(cinv_int16_t *)TO_PTR(p);
		break;
	case T_JINT:
		i = (jint)*(cinv_int32_t *)TO_PTR(p);
		break;
	case T_JLONG:
		l = (jlong)*(cinv_int64_t *)TO_PTR(p);
		break;
	}
	
	jmethodID meth;
	switch (type) {
	case CINV_T_CHAR:
		meth = env->GetMethodID(cls, "<init>", "(B)V");
		if (!meth) return NULL;
		return env->NewObject(cls, meth, b);
	case CINV_T_SHORT:
	case T_JINT:
		meth = env->GetMethodID(cls, "<init>", "(I)V");
		if (!meth) return NULL;
		return env->NewObject(cls, meth, i);
	case CINV_T_INT:
	case CINV_T_LONG:
	case CINV_T_EXTRALONG:
	case CINV_T_PTR:
	case T_JLONG:
		meth = env->GetMethodID(cls, "<init>", "(J)V");
		if (!meth) return NULL;
		return env->NewObject(cls, meth, l);
	case CINV_T_FLOAT:
		meth = env->GetMethodID(cls, "<init>", "(F)V");
		if (!meth) return NULL;
		return env->NewObject(cls, meth, f);
	case CINV_T_DOUBLE:
		meth = env->GetMethodID(cls, "<init>", "(D)V");
		if (!meth) return NULL;
		return env->NewObject(cls, meth, d);
	case T_JSHORT:
		meth = env->GetMethodID(cls, "<init>", "(S)V");
		if (!meth) return NULL;
		return env->NewObject(cls, meth, s);
	}
	
	return NULL;
}
JNIEXPORT jint JNICALL Java_org_cinvoke_Natives_setMemberValueStruct(
	JNIEnv *env, jclass, jlong c, jlong s, jlong i, jstring name, jobject val,
	jint type) {
	CInvContext *ctx = (CInvContext *)TO_PTR(c);
	CInvStructure *st = (CInvStructure *)TO_PTR(s);
	void *inst = (void *)TO_PTR(i);
	const char *chrs = env->GetStringUTFChars(name, NULL);
	if (chrs == NULL) return 0;

	void *p = cinv_structure_instance_getvalue(ctx, st, inst, chrs);
	if (p == NULL) return 0;

	env->ReleaseStringUTFChars(name, chrs);
	
	Java_org_cinvoke_Natives_writeValue(env, NULL, TO_LONG(p), val, type);

	return CINV_SUCCESS;
}
JNIEXPORT jobject JNICALL Java_org_cinvoke_Natives_getMemberValueStruct(
	JNIEnv *env, jclass, jlong c, jlong s, jlong i, jstring name, jclass cls,
	jint type) {
	CInvContext *ctx = (CInvContext *)TO_PTR(c);
	CInvStructure *st = (CInvStructure *)TO_PTR(s);
	void *inst = (void *)TO_PTR(i);
	const char *chrs = env->GetStringUTFChars(name, NULL);
	if (chrs == NULL) return NULL;

	void *p = cinv_structure_instance_getvalue(ctx, st, inst, chrs);
	if (p == NULL) return NULL;

	env->ReleaseStringUTFChars(name, chrs);
	
	return Java_org_cinvoke_Natives_readValue(env, NULL, TO_LONG(p), cls, type);
}
JNIEXPORT jlong JNICALL Java_org_cinvoke_Natives_getMemberPtrStruct(
	JNIEnv *env, jclass, jlong c, jlong s, jlong i, jstring name) {
	CInvContext *ctx = (CInvContext *)TO_PTR(c);
	CInvStructure *st = (CInvStructure *)TO_PTR(s);
	void *inst = (void *)TO_PTR(i);
	const char *chrs = env->GetStringUTFChars(name, NULL);
	if (chrs == NULL) return 0;

	void *p = cinv_structure_instance_getvalue(ctx, st, inst, chrs);
	if (p == NULL) return 0;

	env->ReleaseStringUTFChars(name, chrs);
	
	return TO_LONG(p);
}
JNIEXPORT jint JNICALL Java_org_cinvoke_Natives_finishStruct(
	JNIEnv *env, jclass, jlong c, jlong s) {
	CInvContext *ctx = (CInvContext *)TO_PTR(c);
	CInvStructure *st = (CInvStructure *)TO_PTR(s);

	return cinv_structure_finish(ctx, st);
}
JNIEXPORT jint JNICALL Java_org_cinvoke_Natives_sizeStruct(
	JNIEnv *env, jclass, jlong c, jlong s) {
	CInvContext *ctx = (CInvContext *)TO_PTR(c);
	CInvStructure *st = (CInvStructure *)TO_PTR(s);

	int ret;
	if (!cinv_structure_getsize(ctx, st, &ret))
		return -1;
	return ret;
}
JNIEXPORT jint JNICALL Java_org_cinvoke_Natives_deleteStruct(
	JNIEnv *env, jclass, jlong c, jlong s) {
	CInvContext *ctx = (CInvContext *)TO_PTR(c);
	CInvStructure *st = (CInvStructure *)TO_PTR(s);

	return cinv_structure_delete(ctx, st);
}

struct ud {
	JNIEnv *env;
	jobject ref;
	int numparms;
	jclass *pclasses;
	int *ptypes;
	int rettype;
	bool hasretval;
};

void cbfunc(CInvFunction *f, void *parameters[],
	void *returnout, void *userdata) {
	ud *u = (ud *)userdata;

	JNIEnv *env = u->env;
	jobject val = u->ref;
	jclass *pclasses = u->pclasses;
	int *ptypes = u->ptypes;
	int numparms = u->numparms;
	int rettype = u->rettype;
	bool hasretval = u->hasretval;

	jclass cls = env->GetObjectClass(val);
	if (!cls) return;
	jclass objcls = env->FindClass("java/lang/Object");
	if (!objcls) return;

	jmethodID meth = env->GetMethodID(cls, "cbfunc",
		"([Ljava/lang/Object;)Ljava/lang/Object;");
	if (!meth) return;

	jobjectArray parr = (jobjectArray)env->NewObjectArray(numparms,
		objcls, NULL);
	if (!parr) return;
	for (int i = 0; i < numparms; i++) {
		env->SetObjectArrayElement(parr, i,
			Java_org_cinvoke_Natives_readValue(env, NULL,
				TO_LONG(parameters[i]), pclasses[i], ptypes[i]));
		if (env->ExceptionOccurred()) return;
	}

	jobject retval = env->CallObjectMethod(val, meth, parr);
	if (env->ExceptionOccurred())
		return;

	if (hasretval)
		Java_org_cinvoke_Natives_writeValue(env, NULL, TO_LONG(returnout),
			retval, rettype);
}

JNIEXPORT jlong JNICALL Java_org_cinvoke_Natives_createCallback(
	JNIEnv *env, jclass, jlong c, jlong f, jobject cbthunk,
	jobjectArray pclasses, jintArray ptypes, jboolean hasret, jint rettype) {
	CInvContext *ctx = (CInvContext *)TO_PTR(c);
	CInvFunction *func = (CInvFunction *)TO_PTR(f);
	jobject ref = NULL;
	ud *u = NULL;
	int numparms = 0;
	int *typearr = NULL;
	jclass *clsarr = NULL;
	jint *tel = NULL;

	ref = env->NewGlobalRef(cbthunk);
	if (!ref)
		goto error;
	numparms = env->GetArrayLength(pclasses);
	if (env->ExceptionOccurred())
		goto error;
	typearr = (int *)malloc(sizeof(int) * numparms);
	if (!typearr)
		goto error;
	tel = env->GetIntArrayElements(ptypes, NULL);
	if (!tel)
		goto error;
	for (int i = 0; i < numparms; i++)
		typearr[i] = tel[i];
	env->ReleaseIntArrayElements(ptypes, tel, 0);
	clsarr = (jclass*)malloc(sizeof(jclass) * numparms);
	if (!clsarr)
		goto error;
	for (int i = 0; i < numparms; i++)
		clsarr[i] = NULL;
	for (int i = 0; i < numparms; i++) {
		jclass cls = (jclass)env->GetObjectArrayElement(pclasses, i);
		if (!cls)
			goto error;
		clsarr[i] = (jclass)env->NewGlobalRef(cls);
		if (!clsarr[i])
			goto error;
	}
	u = new ud();
	if (!u)
		goto error;

	u->env = env;
	u->ref = ref;
	u->numparms = numparms;
	u->rettype = rettype;
	u->hasretval = hasret?true:false;
	u->ptypes = typearr;
	u->pclasses = clsarr;
	return TO_LONG(cinv_callback_create(ctx, func, u, cbfunc));
error:
	if (ref)
		env->DeleteGlobalRef(ref);
	free(typearr);
	if (clsarr) {
		for (int i = 0; i < u->numparms; i++) {
			if (clsarr[i])
				env->DeleteGlobalRef(clsarr[i]);
		}
		free(clsarr);
	}
	delete u;
	return 0;
}
JNIEXPORT jlong JNICALL Java_org_cinvoke_Natives_getEPCallback(
	JNIEnv *env, jclass, jlong c, jlong b) {
	CInvContext *ctx = (CInvContext *)TO_PTR(c);
	CInvCallback *cb = (CInvCallback *)TO_PTR(b);

	return TO_LONG(cinv_callback_getentrypoint(ctx, cb));
}
JNIEXPORT jint JNICALL Java_org_cinvoke_Natives_deleteCallback(
	JNIEnv *env, jclass, jlong c, jlong b) {
	CInvContext *ctx = (CInvContext *)TO_PTR(c);
	CInvCallback *cb = (CInvCallback *)TO_PTR(b);

	ud *u = (ud *)cb->userdata;
	u->env->DeleteGlobalRef(u->ref);
	for (int i = 0; i < u->numparms; i++)
		u->env->DeleteGlobalRef(u->pclasses[i]);
	free(u->pclasses);
	free(u->ptypes);
	free(u);

	return cinv_callback_delete(ctx, cb);
}

JNIEXPORT jint JNICALL Java_org_cinvoke_Natives_sizeofBasic(
	JNIEnv *env, jclass, jint type) {
	int sz = 0;

	switch (type) {
	case CINV_T_CHAR:
		sz = 1;
		break;
	case CINV_T_SHORT:
		sz = sizeof(short);
		break;
	case CINV_T_INT:
		sz = sizeof(int);
		break;
	case CINV_T_LONG:
		sz = sizeof(long);
		break;
	case CINV_T_EXTRALONG:
		sz = sizeof(long long);
		break;
	case CINV_T_PTR: 
		sz = sizeof(void *);
		break;
	case CINV_T_FLOAT:
		sz = sizeof(float);
		break;
	case CINV_T_DOUBLE:
		sz = sizeof(double);
		break;
	case T_JSHORT:
		sz = sizeof(jshort);
		break;
	case T_JINT:
		sz = sizeof(jint);
		break;
	case T_JLONG:
		sz = sizeof(jlong);
		break;
	}

	return sz;
}
JNIEXPORT jstring JNICALL Java_org_cinvoke_Natives_ptrToStringUTF8(
	JNIEnv *env, jclass, jlong ptr) {
	return env->NewStringUTF((char *)TO_PTR(ptr));
}
JNIEXPORT jstring JNICALL Java_org_cinvoke_Natives_ptrToStringUnicode(
	JNIEnv *env, jclass, jlong ptr, jint len) {
	if (len < 0) {
		jchar *t = (jchar *)TO_PTR(ptr);
		int i = 0;
		while (*t) { i++; t++; }
		return env->NewString((jchar *)TO_PTR(ptr), i);
	} else
		return env->NewString((jchar *)TO_PTR(ptr), len);
}
JNIEXPORT jlong JNICALL Java_org_cinvoke_Natives_stringToPtrUTF8(
	JNIEnv *env, jclass, jstring str) {
	jsize len = env->GetStringUTFLength(str);
	if (env->ExceptionOccurred()) return 0;
	char *ret = (char *)malloc(len + 1);
	if (!ret) return 0;
	const char *chrs = env->GetStringUTFChars(str, NULL);
	if (!chrs) {
		free(ret);
		return 0;
	}
	memcpy(ret, chrs, len);
	env->ReleaseStringUTFChars(str, chrs);
	ret[len] = 0;
	return TO_LONG(ret);
}
JNIEXPORT jlong JNICALL Java_org_cinvoke_Natives_stringToPtrUnicode(
	JNIEnv *env, jclass, jstring str) {
	jsize len = env->GetStringLength(str);
	if (env->ExceptionOccurred()) return 0;
	jchar *ret = (jchar *)malloc((len + 1) * sizeof(jchar));
	if (!ret) return 0;
	const jchar *chrs = env->GetStringChars(str, NULL);
	if (!chrs) {
		free(ret);
		return 0;
	}
	memcpy(ret, chrs, len * sizeof(jchar));
	env->ReleaseStringChars(str, chrs);
	ret[len] = 0;
	return TO_LONG(ret);
}
