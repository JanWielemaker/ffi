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
import java.io.Serializable;

/**
 * Used to send or read values of the C type 'long'.  The size of integer values
 * in C can vary with the current native platform.  Declaring native interfaces
 * with this type will always marshal the corresponding values as the correct
 * size.
 */
public final class NativeLong implements Serializable {
	/**
	 * Initializes a NativeLong instance.
	 * @param val The value to marshal.  Note that not all the bits in the long
	 * value are guaranteed to be used by the called platform.
	 */
	public NativeLong(long val) {
		_val = val;
	}
	/**
	 * Returns a marshaled value.
	 * @return The marshaled value.  Note that not all the bits in the long
	 * value are guaranteed to be used by the called platform.
	 */
	public long longValue() {
		return _val;
	}

	public boolean equals(Object o) {
		if (o == null) return false;
		if (this == o) return true;
		if (!(NativeLong.class.isInstance(o))) return false;
		NativeLong ni = (NativeLong)o;
		return _val == ni._val;
	}
	public int hashCode() {
		return new Long(_val).hashCode();
	}
	public String toString() {
		return new Long(_val).toString();
	}
	private long _val;

	private static final long serialVersionUID = 1;
}

