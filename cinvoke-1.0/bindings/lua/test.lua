require("cinvoke_lua")

libc = clibrary.new("libc.so.6")

getpass = libc:get_function(Cstring, "getpass", Cstring)
strcpy = libc:get_function(Cptr, "strcpy", cinv.array(Cchar), Cstring)

pass = getpass("Enter a password: ")
print("You entered " .. pass)

buf = { "a", "b", "c" }
strcpy(buf, "ef")

print(cinv.chararray_to_string(buf, 2))
