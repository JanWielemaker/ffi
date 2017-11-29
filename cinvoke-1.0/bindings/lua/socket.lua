require("cinvoke_lua")

libc = clibrary.new("libc.so.6")

sockaddr_in = cstructure.new(
	Cshort, "sin_family",
	Cshort, "sin_port",
	Cint, "in_addr",
	Cint64, "sin_zero")

sock = libc:get_function(Cint , "socket", Cint, Cint, Cint)
lstn = libc:get_function(Cint,"listen", Cint, Cint)
bnd = libc:get_function(Cint, "bind", Cint, cinv.array(sockaddr_in), Cint)
acpt = libc:get_function(Cint, "accept", Cint, cinv.array(sockaddr_in), cinv.array(Cint))
inet_ntoa = libc:get_function(Cstring, "inet_ntoa", Cint)
close = libc:get_function(Cint, "close", Cint)

PORT = 47115 -- 3000 in reverse byte order

nsock = sock(2, 1, 0)
myaddr = {
	sin_family = 2,
	sin_port = PORT,
	in_addr = 0,
	sin_zero = 0
}

bnd(nsock, { myaddr }, 16)
lstn(nsock, 10)
print ("Listening...")
while 1 do
	claddr = {}
	claddr[1] = {
		sin_family = 0,
		sin_port = 0,
		in_addr = 0,
		sin_zero = 0
	}
	len = 16
	clsock = acpt(nsock, claddr, {len})
	
	print("Got a connection from " .. inet_ntoa(claddr[1].in_addr))
	
	close(clsock)
end
