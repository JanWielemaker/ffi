/* DO NOT EDIT THIS FILE - it is machine generated */

#ifndef __org_cinvoke_Natives__
#define __org_cinvoke_Natives__

#include <jni.h>

#ifdef __cplusplus
extern "C"
{
#endif

JNIEXPORT jlong JNICALL Java_org_cinvoke_Natives_createContext (JNIEnv *env, jclass);
JNIEXPORT jstring JNICALL Java_org_cinvoke_Natives_getError (JNIEnv *env, jclass, jlong);
JNIEXPORT jint JNICALL Java_org_cinvoke_Natives_deleteContext (JNIEnv *env, jclass, jlong);
JNIEXPORT jlong JNICALL Java_org_cinvoke_Natives_createLibrary (JNIEnv *env, jclass, jlong, jstring);
JNIEXPORT jlong JNICALL Java_org_cinvoke_Natives_loadEPLibrary (JNIEnv *env, jclass, jlong, jlong, jstring);
JNIEXPORT jint JNICALL Java_org_cinvoke_Natives_deleteLibrary (JNIEnv *env, jclass, jlong, jlong);
JNIEXPORT jlong JNICALL Java_org_cinvoke_Natives_createFunction (JNIEnv *env, jclass, jlong, jint, jstring, jstring);
JNIEXPORT jobject JNICALL Java_org_cinvoke_Natives_invokeFunction (JNIEnv *env, jclass, jlong, jlong, jlong, jobjectArray, jintArray, jclass, jint);
JNIEXPORT jint JNICALL Java_org_cinvoke_Natives_deleteFunction (JNIEnv *env, jclass, jlong, jlong);
JNIEXPORT jlong JNICALL Java_org_cinvoke_Natives_createStruct (JNIEnv *env, jclass, jlong);
JNIEXPORT jint JNICALL Java_org_cinvoke_Natives_addValueMemberStruct (JNIEnv *env, jclass, jlong, jlong, jstring, jint);
JNIEXPORT jint JNICALL Java_org_cinvoke_Natives_addStructMemberStruct (JNIEnv *env, jclass, jlong, jlong, jstring, jlong);
JNIEXPORT jlong JNICALL Java_org_cinvoke_Natives_alloc (JNIEnv *env, jclass, jint);
JNIEXPORT void JNICALL Java_org_cinvoke_Natives_free (JNIEnv *env, jclass, jlong);
JNIEXPORT jint JNICALL Java_org_cinvoke_Natives_sizeofBasic (JNIEnv *env, jclass, jint);
JNIEXPORT jlong JNICALL Java_org_cinvoke_Natives_writeValue (JNIEnv *env, jclass, jlong, jobject, jint);
JNIEXPORT jobject JNICALL Java_org_cinvoke_Natives_readValue (JNIEnv *env, jclass, jlong, jclass, jint);
JNIEXPORT jint JNICALL Java_org_cinvoke_Natives_setMemberValueStruct (JNIEnv *env, jclass, jlong, jlong, jlong, jstring, jobject, jint);
JNIEXPORT jobject JNICALL Java_org_cinvoke_Natives_getMemberValueStruct (JNIEnv *env, jclass, jlong, jlong, jlong, jstring, jclass, jint);
JNIEXPORT jlong JNICALL Java_org_cinvoke_Natives_getMemberPtrStruct (JNIEnv *env, jclass, jlong, jlong, jlong, jstring);
JNIEXPORT jint JNICALL Java_org_cinvoke_Natives_finishStruct (JNIEnv *env, jclass, jlong, jlong);
JNIEXPORT jint JNICALL Java_org_cinvoke_Natives_sizeStruct (JNIEnv *env, jclass, jlong, jlong);
JNIEXPORT jint JNICALL Java_org_cinvoke_Natives_deleteStruct (JNIEnv *env, jclass, jlong, jlong);
JNIEXPORT jlong JNICALL Java_org_cinvoke_Natives_createCallback (JNIEnv *env, jclass, jlong, jlong, jobject, jobjectArray, jintArray, jboolean, jint);
JNIEXPORT jlong JNICALL Java_org_cinvoke_Natives_getEPCallback (JNIEnv *env, jclass, jlong, jlong);
JNIEXPORT jint JNICALL Java_org_cinvoke_Natives_deleteCallback (JNIEnv *env, jclass, jlong, jlong);
JNIEXPORT jstring JNICALL Java_org_cinvoke_Natives_ptrToStringUTF8 (JNIEnv *env, jclass, jlong);
JNIEXPORT jstring JNICALL Java_org_cinvoke_Natives_ptrToStringUnicode (JNIEnv *env, jclass, jlong, jint);
JNIEXPORT jlong JNICALL Java_org_cinvoke_Natives_stringToPtrUTF8 (JNIEnv *env, jclass, jstring);
JNIEXPORT jlong JNICALL Java_org_cinvoke_Natives_stringToPtrUnicode (JNIEnv *env, jclass, jstring);
#undef org_cinvoke_Natives_T_JSHORT
#define org_cinvoke_Natives_T_JSHORT -1L
#undef org_cinvoke_Natives_T_JINT
#define org_cinvoke_Natives_T_JINT -2L
#undef org_cinvoke_Natives_T_JLONG
#define org_cinvoke_Natives_T_JLONG -3L
#undef org_cinvoke_Natives_T_CHAR
#define org_cinvoke_Natives_T_CHAR 0L
#undef org_cinvoke_Natives_T_SHORT
#define org_cinvoke_Natives_T_SHORT 1L
#undef org_cinvoke_Natives_T_INT
#define org_cinvoke_Natives_T_INT 2L
#undef org_cinvoke_Natives_T_LONG
#define org_cinvoke_Natives_T_LONG 3L
#undef org_cinvoke_Natives_T_EXTRALONG
#define org_cinvoke_Natives_T_EXTRALONG 4L
#undef org_cinvoke_Natives_T_FLOAT
#define org_cinvoke_Natives_T_FLOAT 5L
#undef org_cinvoke_Natives_T_DOUBLE
#define org_cinvoke_Natives_T_DOUBLE 6L
#undef org_cinvoke_Natives_T_PTR
#define org_cinvoke_Natives_T_PTR 7L

#ifdef __cplusplus
}
#endif

#endif /* __org_cinvoke_Natives__ */