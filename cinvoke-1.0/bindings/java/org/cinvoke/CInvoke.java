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

/**
 * Provides interfaces and related utility methods for calling C libraries.
 * The CInvoke class is the main public class in the C/Invoke Java binding.
 * In addition to the load() methods, which connect Java interfaces to native
 * shared libraries, it contains methods which can marshal parameters or
 * return values in situations where the library cannot determine the correct
 * marshaling behavior on its own.
 */
public final class CInvoke {
	/**
	 * An enumeration of values representing different calling conventions.
	 */
	public class CC {
		/**
		 * The default calling convention on the current platform.
		 */
		public static final int DEFAULT = -1;
		/**
		 * The cdecl calling convention is the most common convention on x86.
		 */
		public static final int CDECL = 0;
		/**
		  * The stdcall convention is used by the Microsoft Windows API.
		  */
		public static final int STDCALL = 1;
		/**
		 * The fastcall convention is rarely used under Microsoft Windows.
		 */
		public static final int FASTCALL = 2;
	}
	/**
	 * An enumeration which determines the encoding to use when marshalling
	 * strings.
	 */
	public class ENC {
		/**
		 * Marshal strings using the Java UTF-8 variant.
		 */
		public static final int UTF8 = 0;
		/**
		 * Marshal strings as two-byte Unicode values.
		 */
		public static final int UNICODE = 1;
	}

	private static Natives _n;

	static {
		_n = new Natives();
	}

	/** 
	 * Loads a native C library and attaches it to the given Java interface.
	 * See the documentation for the {@link #load(String, Class, int, int)}
	 * overload for more information.  Using this overload calls functions
	 * with the default calling convention and the UTF-8 string encoding.
	 * @param libname The platform-specific name of the library to load.
	 * @param iface The interface to attach to the library.
	 * @return An object instance which implements the given interface.  This
	 * object acts a proxy to the native C library.
	 */
	public static Object load(String libname, Class iface) {
		return load(libname, iface, CC.DEFAULT);
	}
	/** 
	 * Loads a native C library and attaches it to the given Java interface.
	 * See the documentation for the {@link #load(String, Class, int, int)}
	 * overload for more information.  Using this overload calls functions
	 * with the UTF-8 string encoding.
	 * @param libname The platform-specific name of the library to load.
	 * @param iface The interface to attach to the library.
	 * @param callconv The calling convention to use on functions in this
	 * library, from the {@link CInvoke.CC} enumeration.
	 * @return An object instance which implements the given interface.  This
	 * object acts a proxy to the native C library.
	 */
	public static Object load(String libname, Class iface, int callconv) {
		return load(libname, iface, CC.DEFAULT, ENC.UTF8);
	}
	/** 
	 * Loads a native C library and attaches it to the given Java interface.
	 * Each method in the given interface should correspond to a function
	 * symbol in the given library. However, individual symbols are not
	 * resolved until their corresponding methods are called for the first time.
	 * The signature of the interface method determines how
	 * parameters and return values are marshaled to and from C.  The following
	 * types are allowed in method signatures:
	 * <li> <i>byte, Byte</i>: Marshaled as a C 'char' type.
	 * <li> <i>short, Short</i>: Marshaled as a 2-byte integer.
	 * <li> <i>int, Integer</i>: Marshaled as a 4-byte integer.
	 * <li> <i>long, Long</i>: Marshaled as an 8-byte integer.
	 * <li> <i>float, Float</i>: Marshaled as a C 'float' type.
	 * <li> <i>double, Double</i>: Marshaled as a C 'double' type.
	 * <li> <i>{@link NativeShort}</i>: Marshaled as a C 'short' type.
	 * <li> <i>{@link NativeInt}</i>: Marshaled as a C 'int' type.
	 * <li> <i>{@link NativeLong}</i>: Marshaled as a C 'long' type.
	 * <li> <i>{@link NativeLongLong}</i>: Marshaled as a C 'long long' type.
	 * <li> <i>{@link Ptr}</i>: Marshaled a C pointer value (i.e. a void*, or
	 * any other pointer of the same size).
	 * <li> <i>String</i>: Marshaled as a constant string pointer, with either
	 * UTF-8 or Unicode encoding.  When used as a return value, marshaling
	 * continues up until the first 0 character.
	 * <li> <i>Any Interface</i>: If an interface type is specified for a
	 * parameter, then an instance of the interface will be marshaled as a
	 * function pointer.  The interface specified should have one method
	 * defined.  This method will be
	 * called back when unmanaged code calls the function pointer.  Therefore,
	 * the prototype of this method should match the callback protoype.
	 * Arrays and callbacks cannot be used as parameter types in callback
	 * prototypes, and Strings cannot be used as parameters or as the return
	 * type.  Callbacks are currently only supported when all the calls to a
	 * callback are made from within the calling function.  Passing a managed
	 * function pointer to a C function which stores the value and calls it
	 * later in another thread may result in undefined behavior.  Interfaces
	 * cannot be used as return types.
	 * <li> <i>Any Array Type</i>: Arrays of any other valid type are marshaled
	 * as pointers to C array values.  Arrays, unlike Strings, are mutable;
	 * their elements are writable by unmanaged code, and after a C function
	 * returns, any value written to an array element is marshaled back to
	 * a corresponding Java value.  This means you can use arrays whenever
	 * pointer or pass-by-reference semantics are required.  However, arrays of
	 * interfaces or Strings are not supported, and using an array as a return
	 * type is not supported (use {@link #ptrToString(Ptr, int, int) ptrToString}
	 * or {@link #ptrToArray(Ptr, Class, int) ptrToArray} instead).
	 * <li> <i>Any Class</i>: Class types other than those listed above are
	 * marshaled as C structures, with each public field of a class
	 * corresponding to a structure member.  Unfortunately, there is a twist to
	 * declaring classes which are compatible with C structures: in C, the order
	 * of the structure members is very important, but Java provides no method
	 * of determining the order class fields are declared in.  Thus, in order
	 * to correctly marshal C structures, classes should be declared with the
	 * names of the fields in the alphabetical order which matches the order in
	 * the c struct. For example, to marshal the following structure definition:
	 * <pre>
struct st {
	short mys;
	int myi
	float myfl;
};
	 * </pre>
	 * the Java class should be declared:
	 * <pre>
class st {
	public NativeShort a_mys;
	public NativeInt b_myi;
	public float c_myf;
}
	 * </pre>
	 * The prefixes on the members ensure that the alphabetic order of the
	 * class fields corresponds to the C structure member order.
	 * <p>Structures cannot contain members
	 * that are arrays, Strings, or interfaces.  Members which have class types
	 * are treated as embedded structure values.  Structures can never be
	 * passed or returned by value, only inside arrays.  To pass a pointer to
	 * a structure to a function, pass an array with a length of 1.  To return
	 * a pointer to a function, return a Ptr value and convert it to an array
	 * of structures with {@link #ptrToArray(Ptr, Class, int) ptrToArray}.
	 * @param libname The platform-specific name of the library to load.
	 * This is usually the name of a file containing the shared object file,
	 * i.e. mylib.dll or /lib/libmylib.so.  The search path for library files
	 * is also implementation and system-defined; specifying the full path to
	 * a library file usually yields the most portable behavior.
	 * @param iface The interface to attach to the library.  The returned
	 * object implements this interface.
	 * @param callconv The calling convention to use on functions in this
	 * library, from the {@link CInvoke.CC} enumeration.
	 * @param encoding The encoding to use when marshaling strings in and out
	 * of this library, from the {@link CInvoke.ENC} enumeration.
	 * @return An object instance which implements the given interface.  This
	 * object acts a proxy to the native C library.  When a method in the given
	 * interface is called on this object, the name of the method is used as
	 * a symbol name to be looked up in the library.  The parameters to the
	 * method are translated to their C equivalents, the C function is called,
	 * and the return value (and any array values) are translated back to
	 * Java objects.
	 */
	public static Object load(String libname, Class iface, int callconv,
		int encoding) {
		return Proxy.newProxyInstance(iface.getClassLoader(),
			new Class[] { iface }, new CInvProxy(libname, callconv, encoding));
	}

	/** 
	 * Converts a pointer to an unmanaged buffer to a Java string value.
	 * @param ptr A pointer value which points to an array of bytes or
	 * Unicode characters.
	 * @param numchars The number of characters in the string.  Specify a
	 * negative value
	 * to stop conversion at the first 0 character.  This parameter is ignored
	 * when using the UTF-8 encoding, which always stops at the first 0 byte.
	 * @param encoding The encoding to use, must be one of the values from the
	 * {@link CInvoke.ENC} enumeration.
	 * @return The completed String value.
	 */
	public static String ptrToString(Ptr ptr, int numchars, int encoding) {
		synchronized (_n) {
			return _n.ptrToString(ptr, numchars, encoding);
		}
	}
	/** 
	 * Converts a pointer to a native array to a Java array value.
	 * @param ptr A pointer value which points to a contiguous array of
	 * unmanaged values.
	 * @param eltype The type of the elements of the array to create.  Must
	 * be either a class with public fields (marshalled as a C structure) or
	 * a "basic" type (Byte, Short, Int, Long, Float, Double, NativeShort,
	 * NativeInt, NativeLong, NativeLongLong, or Ptr).
	 * @param num The number of elements in the array.
	 * @return The completed Java array, cast to the Object[] type.
	 */
	public static Object[] ptrToArray(Ptr ptr, Class eltype, int num) {
		synchronized (_n) {
			return _n.ptrToArray(ptr, eltype, num);
		}
	}
	/** 
	 * Utility method which returns the number of bytes required to hold an
	 * instance of the given marshalable type.
	 * @param type The type to retrieve the size of.
	 * @return The number of bytes of unmanaged memory required to hold an
	 * instance of the specified type.
	 */
	public static int getSize(Class type) {
		synchronized (_n) {
			return _n.getSize(type);
		}
	}
	/** 
	 * Utility method which returns the number of bytes required
	 * to hold the given marshalable value.  If the value is an array,
	 * the size returned will be the combined size of all array elements.
	 * @param obj The object to retrieve the size of.
	 * @return The number of bytes of unmanaged memory required to hold the
	 * object.
	 */
	public static int getSize(Object obj) {
		synchronized (_n) {
			return _n.getSize(obj);
		}
	}
}
