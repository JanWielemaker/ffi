import org.cinvoke.*;

class Test {
	interface libc {
		String getpass(String prompt);
		Ptr strcpy(byte[] dest, String src);
	}

	public static void main(String[] args) {
		String libname;
		String osname = System.getProperty("os.name");
		if (osname.equals("Mac OS X"))
			libname = "libc.dylib";
		else if (osname.equals("SunOS"))
			libname = "libc.so.1";
		else
			libname = "libc.so.6";
		libc c = (libc)CInvoke.load(libname, libc.class);

		System.out.println("You entered: " + c.getpass("Enter password: "));

		byte[] arr = new byte[] { 'a', 'b', 'c', 'd' };
		c.strcpy(arr, "ef");
		for (int i = 0; i < arr.length; i++)
			System.out.print(arr[i] + " ");
		System.out.println();
	}
}
