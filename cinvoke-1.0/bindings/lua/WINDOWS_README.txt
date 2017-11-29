The Lua binding is now supported under MS Windows.  This
is how it works on my machine:

1) Download the following packages from luabinaries (http://luabinaries.luaforge.net/):

lua5_1_Win32_bin.tar.gz
lua5_1_Win32_dll.tar.gz

and un-tar them both to c:\.

You should also add c:\lua5.1\bin\Win32 to your PATH.

2) Build the cinvoke_lua.sln solution (I use VS 2003, 2005 will probably work as well).

At this point I can do the following from this directory:

> copy Debug\cinvoke_lua.dll .
> lua5.1 win32test.lua

Support for mingw32 compilation coming soon.