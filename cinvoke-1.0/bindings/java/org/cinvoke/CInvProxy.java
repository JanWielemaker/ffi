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
import java.lang.reflect.*;

class CInvProxy implements InvocationHandler {
	private long _lib = 0;
	private int _cc;
	private int _encoding;
	private Natives _n;

	public CInvProxy(String libname, int cc, int encoding) {
		_n = new Natives();
		_lib = Natives.createLibrary(_n._ctx, libname);
		if (_lib == 0) {
			try {
				_n.fail();
			} finally {
				_n.close();
			}
		}

		_cc = cc;
		_encoding = encoding;
	}

	protected void finalize() throws Throwable {
		if (_lib != 0) {
			Natives.deleteLibrary(_n._ctx, _lib);
			_lib = 0;
		}
		_n.close();
	}

	public Object invoke(Object proxy, Method method, Object[] args)
		throws Throwable {
		synchronized (_n) {
			Natives.NativeMethod meth = _n.createNativeMethod(method, _lib,
				_cc);
			Class returncls = method.getReturnType();
			if (returncls.equals(Void.TYPE))
				returncls = null;
			Class realretcls = returncls;
			if (String.class.equals(returncls))
				realretcls = Ptr.class;

			Class[] pclasses = method.getParameterTypes();
			Object[] params = marshalParams(method, pclasses, args);
			try {
				Object ret;
				try {
					ret = Natives.invokeFunction(_n._ctx, meth.func, meth.ep,
						params, meth.types, realretcls, meth.rettype);
				} catch (Exception ex) {
					if (ex.getMessage().equals("invoke failed")) {
						_n.fail();
						return null;
					} else
						throw ex;
				}

				for (int i = 0; i < pclasses.length; i++) {
					if (pclasses[i].isArray()) {
						_n.unmarshalArray(((Ptr)params[i]).longValue(), args[i],
							pclasses[i].getComponentType());
					}
				}

				if (returncls != null)
					return unmarshalReturnValue(ret, returncls);
				else
					return null;
			} finally {
				freeParams(method, params, args);
			}
		}
	}

	private Object[] marshalParams(Method method, Class[] pclasses,
		Object[] rawparams) {
		Object[] ret = new Object[rawparams.length];

		for (int i = 0; i < ret.length; i++) {
			if (pclasses[i].isArray())
				ret[i] = _n.marshalArray(pclasses[i].getComponentType(),
						rawparams[i]);
			else if (pclasses[i].isInterface()) {
				ret[i] = new Ptr(_n.createCB(rawparams[i], pclasses[i], _cc));
			} else if (pclasses[i].equals(String.class))
				ret[i] = _n.marshalString((String)rawparams[i], _encoding);
			else
				ret[i] = _n.marshalBasic(rawparams[i], pclasses[i]);
		}

		return ret;
	}

	private void freeParams(Method method, Object[] params,
		Object[] rawparams) {
		Class[] pclasses = method.getParameterTypes();
		for (int i = 0; i < params.length; i++) {
			if (pclasses[i].isInterface()) {
				_n.deleteCB(((Ptr)params[i]).longValue());
			} else if (pclasses[i].isArray() ||
					pclasses[i].equals(String.class)) {
				Natives.free(((Ptr)params[i]).longValue());
			}
		}
	}

	private Object unmarshalReturnValue(Object ret, Class type) {
		if (type.equals(String.class)) {
			Ptr p = (Ptr)ret;
			return _n.unmarshalString(p.longValue(), _encoding);
		} else
			return ret;
	}

}
