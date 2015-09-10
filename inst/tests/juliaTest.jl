print("Julia started\n")

import JSON

server = listen(1234)
sock = accept(server)
write("accept OK\n")
while true
    bb = JSON.parse(sock); st = string(typeof(bb))
    write("got an object from JSON on socket: $bb \n")
    ## now we should do something: then
    aa = Array(Any, 1)
    aa[1] = "Reply"
    push!(aa, bb)
    JSON.print(sock, aa)
    write(sock, "\n")
    flush(sock)
    write("called JSON.print on the return object \n")
end
