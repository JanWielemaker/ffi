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

package org.cinvoke;
import java.util.*;
import java.lang.reflect.*;

class Natives {
	static {
		System.loadLibrary("cinvoke_java");
	}

	public static native long createContext();
	public static native String getError(long ctx);
	public static native int deleteContext(long ctx);
	public static native long createLibrary(long ctx, String path);
	public static native long loadEPLibrary(long ctx, long lib, String name);
	public static native int deleteLibrary(long ctx, long lib);
	public static native long createFunction(long ctx, int cc, String retfmt,
		String paramfmt);
	public static native Object invokeFunction(long ctx, long func, long ep,
		Object[] params, int[] types, Class retcls, int rettype);
	public static native int deleteFunction(long ctx, long func);
	public static native long createStruct(long ctx);
	public static native int addValueMemberStruct(long ctx, long strct,
		String name, int type);
	public static native int addStructMemberStruct(long ctx, long strct,
		String name, long type);
	public static native long alloc(int sz);
	public static native void free(long m);
	public static native int sizeofBasic(int type);
	public static native long writeValue(long m, Object val, int type);
	public static native Object readValue(long m, Class cls, int type);
	public static native int setMemberValueStruct(long ctx, long strct,
		long m, String name, Object val, int type);
	public static native Object getMemberValueStruct(long ctx, long strct,
		long m, String name, Class cls, int type);
	public static native long getMemberPtrStruct(long ctx, long strct,
		long m, String name);
	public static native int finishStruct(long ctx, long strct);
	public static native int sizeStruct(long ctx, long strct);
	public static native int deleteStruct(long ctx, long strct);
	public static native long createCallback(long ctx, long func,
		CBThunk cbcls, Class[] pclasses, int[] ptypes, boolean hasret,
		int rettype);
	public static native long getEPCallback(long ctx, long cb);
	public static native int deleteCallback(long ctx, long cb); 
	public static native String ptrToStringUTF8(long ptr);
	public static native String ptrToStringUnicode(long ptr, int numchars);
	public static native long stringToPtrUTF8(String str);
	public static native long stringToPtrUnicode(String str);

	private static final int T_JSHORT = -1;
	private static final int T_JINT = -2;
	private static final int T_JLONG = -3;
	private static final int T_CHAR = 0;
	private static final int T_SHORT = 1;
	private static final int T_INT = 2;
	private static final int T_LONG = 3;
	private static final int T_EXTRALONG = 4;
	private static final int T_FLOAT = 5;
	private static final int T_DOUBLE = 6;
	private static final int T_PTR = 7;

	private final static int STRUCT_TYPE = -999;

	public long _ctx = 0;
	private HashMap _functions;
	private HashMap _structs;
	private HashMap _callbacks;

	public Natives() {
		_ctx = createContext();
		if (_ctx == 0) throw new OutOfMemoryError();

		_functions = new HashMap();
		_structs = new HashMap();
		_callbacks = new HashMap();
	}

	public void close() {
		for (Iterator i = _functions.values().iterator(); i.hasNext();)
			((NativeMethod)i.next()).close();
		for (Iterator i = _structs.values().iterator(); i.hasNext();) 
			((NativeStruct)i.next()).close();
		if (_ctx != 0) {
			deleteContext(_ctx);
			_ctx = 0;
		}
	}

	public void fail() {
		throw new CInvokeError(getError(_ctx));
	}

	class NativeStruct {
		public NativeStruct(long ctx, long st) {
			_ctx = ctx;
			this.st = st;
		}

		public void close() {
			if (st != 0) {
				deleteStruct(_ctx, st);
				st = 0;
			}
		}

		public long st;
		private long _ctx;
	}

	class NativeMethod {
		public NativeMethod(long ctx, long func, long ep, int[] types,
			boolean hasret, int rettype) {
			this.func = func;
			this.ep = ep;
			this.types = types;
			this.hasret = hasret;
			this.rettype = rettype;
			_ctx = ctx;
		}

		public void close() {
			if (func != 0) {
				deleteFunction(_ctx, func);
				func = 0;
			}
		}

		private long _ctx;
		public long func;
		public long ep;
		public int[] types;
		public boolean hasret;
		public int rettype;
	}

	public NativeMethod createNativeMethod(Method method, long lib, int cc) {
		NativeMethod meth = (NativeMethod)_functions.get(method);
		if (meth == null) {
			meth = createNative(method, lib, cc);
			_functions.put(method, meth);
		}
		return meth;
	}
	
	private int gettypeint(Class cls, boolean thrw) {
		if (cls.isArray()) {
			if (cls.isInterface())
				throw new CInvokeError("Arrays of callbacks not supported");
			if (cls.equals(String.class))
				throw new CInvokeError("Arrays of strings not supported");
			return T_PTR;
		} else {
			if (cls.isInterface())
				return T_PTR;
			else if (cls.equals(String.class))
				return T_PTR;
			else if (cls.equals(Byte.class) || cls.equals(Byte.TYPE))
				return T_CHAR;
			else if (cls.equals(Short.class) || cls.equals(Short.TYPE))
				return T_JSHORT;
			else if (cls.equals(Integer.class) || cls.equals(Integer.TYPE))
				return T_JINT;
			else if (cls.equals(Long.class) || cls.equals(Long.TYPE))
				return T_JLONG;
			else if (cls.equals(Float.class) || cls.equals(Float.TYPE))
				return T_FLOAT;
			else if (cls.equals(Double.class) || cls.equals(Double.TYPE))
				return T_DOUBLE;
			else if (cls.equals(NativeShort.class))
				return T_SHORT;
			else if (cls.equals(NativeInt.class))
				return T_INT;
			else if (cls.equals(NativeLong.class))
				return T_LONG;
			else if (cls.equals(NativeLongLong.class))
				return T_EXTRALONG;
			else if (cls.equals(Ptr.class))
				return T_PTR;
			else {
				if (thrw)
					throw new CInvokeError(
					"Passing or returning structures by value not supported");
				else
					return STRUCT_TYPE;
			}
		}
	}

	private char gettypechar(int type) {
		switch (type) {
		case T_JSHORT:
			return '2';
		case T_JINT:
			return '4';
		case T_JLONG:
			return '8';
		case T_CHAR:
			return 'c';
		case T_SHORT:
			return 's';
		case T_INT:
			return 'i';
		case T_LONG:
			return 'l';
		case T_EXTRALONG:
			return 'e';
		case T_FLOAT:
			return 'f';
		case T_DOUBLE:
			return 'd';
		case T_PTR:
			return 'p';
		default:
			throw new UnknownError("unknown type");
		}
	}

	private NativeMethod createNative(Method method, long lib, int cc) {
		long ep = 0;
		if (lib != 0) {
			ep = loadEPLibrary(_ctx, lib, method.getName());
			if (ep == 0)
				fail();
		}
		StringBuffer retfmt = new StringBuffer();
		StringBuffer parmfmt = new StringBuffer();
		boolean hasret = false;
		int rettype = 0;
		Class retcls = method.getReturnType();
		if (!retcls.equals(Void.TYPE)) {
			if (retcls.isArray())
				throw new CInvokeError("returning arrays not supported");
			if (retcls.isInterface())
				throw new CInvokeError("returning callbacks not supported");
			if (lib == 0 && retcls.equals(String.class))
				throw new CInvokeError(
					"returning strings not supported for callback methods");
			hasret = true;
			rettype = gettypeint(retcls, true);
			retfmt.append(gettypechar(rettype));
		}

		Class[] pclasses = method.getParameterTypes();
		int[] types = new int[pclasses.length];
		for (int i = 0; i < types.length; i++) {
			if (lib == 0) {
				if (pclasses[i].isArray())
					throw new CInvokeError(
						"passing arrays not supported for callbacks methods");
				if (pclasses[i].isInterface())
					throw new CInvokeError(
						"passing callbacks not supported for callback methods");
				if (pclasses[i].equals(String.class))
					throw new CInvokeError(
						"passing strings not supported for callback methods");
			}
			types[i] = gettypeint(pclasses[i], true);
			parmfmt.append(gettypechar(types[i]));
		}

		long func = createFunction(_ctx, cc, retfmt.toString(),
			parmfmt.toString());
		if (func == 0)
			fail();
		return new NativeMethod(_ctx, func, ep, types, hasret, rettype);
	}

	public Object unmarshalString(long p, int encoding) {
		if (p == 0) return null;
		if (encoding == CInvoke.ENC.UNICODE)
			return ptrToStringUnicode(p, -1);
		else
			return ptrToStringUTF8(p);
	}

	private class FieldComparator implements Comparator {
		public int compare(Object o1, Object o2) {
			return ((Field)o1).getName().compareTo(((Field)o2).getName());
		}
	}

	private Object unmarshalStruct(long ptr, Class cls) {
		long st = getNativeStruct(cls).st;
		Field[] fields = cls.getFields();
		Arrays.sort(fields, new FieldComparator());
		Object ret;

		try {
			ret = cls.newInstance();
		} catch (IllegalAccessException iae) {
			throw new CInvokeError("Cannot create new instance of class " +
				cls.getName() + " due a security error: " + iae.toString());
		} catch (InstantiationException ie) {
			throw new CInvokeError("Cannot create new instance of class " +
				cls.getName() + " due to error: " + ie.toString());
		}
		for (int i = 0; i < fields.length; i++) {
			Field fld = fields[i];
			Class tcls = fld.getType();
			Object val;
			int type = gettypeint(tcls, false);
			if (type == STRUCT_TYPE) {
				long p = getMemberPtrStruct(_ctx, st, ptr,
					fld.getName());
				if (p == 0) fail();
				val = unmarshalStruct(p, tcls);
			} else {
				val = getMemberValueStruct(_ctx, st, ptr,
					fld.getName(), filterType(tcls), type);
			}
			try {
				fld.set(ret, val);
			} catch (IllegalAccessException iae) {
				throw new CInvokeError(cls.getName() +
					" field access failed: " + iae.toString());
			}
		}
		
		return ret;
	}

	private long marshalStruct(long outp, Object s, Class cls) {
		if (s == null)
			throw new CInvokeError("Invalid null value");
		long st = getNativeStruct(cls).st;
		Field[] fields = cls.getFields();
		Arrays.sort(fields, new FieldComparator());
		for (int i = 0; i < fields.length; i++) {
			Field fld = fields[i];
			Class tcls = fld.getType();
			Object val;
			try {
				val = fld.get(s);
			} catch (IllegalAccessException iae) {
				throw new CInvokeError(cls.getName() +
					" field access failed: " + iae.toString());
			}
			int type = gettypeint(tcls, false);
			if (type == STRUCT_TYPE) {
				long p = getMemberPtrStruct(_ctx, st,
					outp, fld.getName());
				if (p == 0) fail();
				marshalStruct(p, val, tcls);
			} else {
				if (setMemberValueStruct(_ctx, st, outp,
					fld.getName(), val, type) == 0)
					fail();
			}
		}

		int sz = sizeStruct(_ctx, st);
		if (sz == -1) fail();
		return outp + sz;
	}

	public Object marshalBasic(Object o, Class cls) {
		if (o == null) {
			if (cls.equals(Ptr.class))
				return new Ptr(0);
			else
				throw new CInvokeError("Invalid null value");
		} else
			return o;
	}

	public Ptr marshalString(String str, int encoding) {
		if (str == null) return new Ptr(0);

		long l;
		if (encoding == CInvoke.ENC.UNICODE)
			l = stringToPtrUnicode(str);
		else 
			l = stringToPtrUTF8(str);
		if (l == 0)
			throw new OutOfMemoryError();
		return new Ptr(l);
	}

	public Ptr marshalArray(Class eltype, Object val) {
		if (val == null) return new Ptr(0);

		int numels = Array.getLength(val);
		int len = numels * getSize(eltype);
		
		long ret = alloc(len);
		if (ret == 0)
			throw new OutOfMemoryError();
		
		int itype = gettypeint(eltype, false);

		long r = ret;
		for (int i = 0; i < numels; i++) {
			Object o = Array.get(val, i);
			if (itype == STRUCT_TYPE)
				r = marshalStruct(r, o, eltype);
			else
				r = writeValue(r, marshalBasic(o, eltype), itype);
		}
		
		return new Ptr(ret);
	}

	public void unmarshalArray(long ptr, Object arr, Class eltype) {
		if (ptr == 0)
			throw new CInvokeError("Reading array from null pointer");
		eltype = filterType(eltype);
		int numels = Array.getLength(arr);
		
		int itype = gettypeint(eltype, false);
		int elsize = getSize(eltype);

		long p = ptr;
		for (int i = 0; i < numels; i++) {
			Object toset;
			if (itype == STRUCT_TYPE)
				toset = unmarshalStruct(p, eltype);
			else
				toset = readValue(p, eltype, itype);
			Array.set(arr, i, toset);

			p += elsize;
		}
	}

	private NativeStruct getNativeStruct(Class cls) {
		NativeStruct ret = (NativeStruct)_structs.get(cls);

		if (ret == null) {
			if (_structs.containsKey(cls))
				throw new CInvokeError(
					"Structures cannot have themselves as members");
			_structs.put(cls, null);
			try {
				long st = createStruct(_ctx);
				if (st == 0)
					fail();
				Field[] fields = cls.getFields();
				Arrays.sort(fields, new FieldComparator());
				for (int i = 0; i < fields.length; i++) {
					Class tcls = fields[i].getType();
					if (tcls.isArray()) {
						deleteStruct(_ctx, st);
						throw new CInvokeError(
							"Array structure members not supported");
					}
					if (tcls.isInterface()) {
						deleteStruct(_ctx, st);
						throw new CInvokeError(
							"Callback structure members not supported");
					}
					if (tcls.equals(String.class)) {
						deleteStruct(_ctx, st);
						throw new CInvokeError(
							"String structure members not supported");
					}
					int type = gettypeint(tcls, false);
					if (type == STRUCT_TYPE) {
						if (addStructMemberStruct(_ctx, st,
							fields[i].getName(),
							getNativeStruct(tcls).st) == 0) {
							try { fail(); }
							finally { deleteStruct(_ctx, st); }
						}
					} else {
						if (addValueMemberStruct(_ctx, st,
							fields[i].getName(), type) == 0) {
							try { fail(); }
							finally { deleteStruct(_ctx, st); }
						}
					}
				}
				if (finishStruct(_ctx, st) == 0) {
					try { fail(); } finally { deleteStruct(_ctx, st); }
				}
				ret = new NativeStruct(_ctx, st);
				_structs.put(cls, ret);
			} finally {
				if (_structs.get(cls) == null)
					_structs.remove(cls);
			}
		}
		
		return ret;
	}

	private Class filterType(Class t) {
		if (t.equals(Byte.TYPE))
			return Byte.class;
		else if (t.equals(Short.TYPE))
			return Short.class;
		else if (t.equals(Integer.TYPE))
			return Integer.class;
		else if (t.equals(Long.TYPE))
			return Long.class;
		else if (t.equals(Float.TYPE))
			return Float.class;
		else if (t.equals(Double.TYPE))
			return Double.class;
		else
			return t;
	}

	public long createCB(Object obj, Class iface, int cc) {
		Method[] methods = iface.getDeclaredMethods();
		if (methods.length != 1)
			throw new CInvokeError("Interface " + iface.getName() +
				" has " + methods.length + " methods, not 1");
				
		Method method = methods[0];
		NativeMethod meth = createNativeMethod(method, 0, cc);

		Class[] cls = method.getParameterTypes();
		for (int i = 0; i < cls.length; i++)
			cls[i] = filterType(cls[i]);

		long cb = createCallback(_ctx, meth.func, new CBThunk(obj, method),
				cls, meth.types, meth.hasret, meth.rettype);
		if (cb == 0)
			fail();
	
		long ep = getEPCallback(_ctx, cb);
		if (ep == 0) {
			try { fail(); }
			finally { deleteCallback(_ctx, cb); }
		}

		_callbacks.put(new Long(ep), new Long(cb));

		return ep;
	}

	public void deleteCB(long ep) {
		Long cb = (Long)_callbacks.get(new Long(ep));
		if (cb != null)
			deleteCallback(_ctx, cb.longValue());
	}

	public String ptrToString(Ptr ptr, int numchars, int encoding) {
		if (encoding == CInvoke.ENC.UNICODE)
			return ptrToStringUnicode(ptr.longValue(), numchars);
		else
			return ptrToStringUTF8(ptr.longValue());
	}
	public Object[] ptrToArray(Ptr ptr, Class eltype, int num) {
		if (eltype.isInterface())
			throw new CInvokeError("Arrays of callbacks not supported");

		Object ret = Array.newInstance(eltype, num);

		unmarshalArray(ptr.longValue(), ret, eltype);

		return (Object[])ret;
	}
	public int getSize(Class type) {
		int itype = gettypeint(type, false);
		if (itype == STRUCT_TYPE) {
			int ret = sizeStruct(_ctx, getNativeStruct(type).st);
			if (ret == -1)
				fail();
			return ret;
		} else
			return sizeofBasic(itype);
	}
	public int getSize(Object obj) {
		Class cls = obj.getClass();
		if (cls.isArray()) {
			if (cls.isInterface())
				throw new CInvokeError("Arrays of callbacks not supported");
			return Array.getLength(obj) * getSize(cls.getComponentType());
		} else
			return getSize(cls);
	}
}
