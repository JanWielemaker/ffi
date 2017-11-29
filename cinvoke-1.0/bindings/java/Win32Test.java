import org.cinvoke.*;

class Win32Test {
	interface User32 {
		public NativeInt GetSystemMetrics(NativeInt nIndex);
	}
	
	private static final NativeInt SM_CXSCREEN = new NativeInt(0);
	private static final NativeInt SM_CYSCREEN = new NativeInt(1);
	
	public static void main(String[] args) {
		User32 u = (User32)CInvoke.load("User32.dll", User32.class, CInvoke.CC.STDCALL);
		
		System.out.println("Your monitor resolution is " +
			u.GetSystemMetrics(SM_CXSCREEN) + "x" + u.GetSystemMetrics(SM_CYSCREEN));
	}
}

