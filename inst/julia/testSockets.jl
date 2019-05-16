using Pkg

function addSockets()
    try
        write(stderr, "Trying to add Julia package Sockets; expect some messages and some delay\n")
        Pkg.add("Sockets")
        0
    catch err
        write(stderr, "Unable to add package Sockets: ")
        showerror(stderr, err)
        1
    end
end

try
    import Sockets
    exit(0)
catch err
    exit(addSockets())
end
