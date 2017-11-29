require("cinvoke_lua")

user32 = clibrary.new("user32.dll", "stdcall")

MessageBox = user32:get_function(Cint, "MessageBoxA", Cptr,
	Cstring, Cstring, Cint32)

MessageBox(0, "Hello From Lua", "C/Invoke Message Box", 0);
