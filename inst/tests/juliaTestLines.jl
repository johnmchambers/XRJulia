print("Julia started\n")

import JSON

server = listen(1235)
sock = accept(server)
write("accept OK\n")
while true
    bb = readline(sock)
    write("got a line from the socket: $bb \n")
    flush(sock)
    write(sock, "Result: $bb")
    write("Wrote a line back to the socket\n")
end
