mkdir install
copy lib\cinvoke.h install
copy lib\cinvoke-arch.h install
copy lib\arch\cl_x86_win.h install\cinvoke-archspec.h
copy lib\Release\cinvoke.lib install
copy lib\Release-Dll\cinvoke-dll.dll install
copy lib\Release-Dll\cinvoke-dll.lib install
echo #define CINVOKE_DLL_IMPORT > install/cinvoke-dll.h
echo #include "cinvoke.h" >> install/cinvoke-dll.h
