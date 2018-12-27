
rjulia_lib = ENV["RJULIA_LIB"]
push!(LOAD_PATH, rjulia_lib)

using XRJulia

verbose = haskey(ENV, "JuliaVerbose")

import JSON
import Sockets

## start evaluator

## all the ENV's should be in a try
## RJuliaSource = ENV["RJuliaSource"] # set in R when jlProc() object initialized

RJuliaPort = 0
try
    global RJuliaPort = eval(Meta.parse(ENV["RJuliaPort"]))
    if verbose
        write(stderr, "Got port: $RJuliaPort \n")
    end
catch err
    write(stderr, "Couldn't get RJuliaPort\n")
    showerror(stderr, err)
    exit()
end

    
if verbose
  show(string("Starting socket on port ",RJuliaPort))
end

RJuliaServer = ""
try global RJuliaServer = Sockets.listen(RJuliaPort)
    if verbose
        show("Server obtained\n")
    end
catch err
    write(stderr, "Couldn't get RJuliaServer\n")
    showerror(stderr, err)
    exit()
end

RJuliaSocket = ""
try global  RJuliaSocket = Sockets.accept(RJuliaServer)
    if verbose
        show("Socket accepting\n")
    end
catch err
    write(stderr, "Couldn't get Socket\n")
    showerror(stderr, err)
    exit()
end


## the server loop:  reads a JSON object which needs to be an RJulia command
if verbose
    show("Starting parse-eval loop")
end
nInternErrors = 0

while true

    cmd = ["undef", "undef", true, true]
    value = NaN
    try
        if(eof(RJuliaSocket)) # can't help R closing sometimes w/o sending Quit to evaluator
            exit()
        end
        cmd = JSON.parse(RJuliaSocket)
        if verbose
            show(string("JSON parsed cmd: ", cmd))
        end
    catch err
        write(stderr, "Julia Error/Interrupt in reading command: ")
        showerror(stderr, err)
        ##### TODO:  distinguish error from interrupt here
        ## nInternErrors += 1
        ## if(nInternErrors > 5)
        ##     write(stderr, "Too many internal errors: quitting\n")
        ##     exit()
        ## end
        continue
    end
    try
        value = RJuliaCommand(cmd)
        if verbose
            show(string("Command returned: ", typeof(value)))
        end
    catch err
        if verbose
            write(stderr, "Julia Error in evaluating command: ")
            showerror(stderr, err)
        end
        ## unlike the processing type errors, this is presumably a user error
        ## and is communicated back to R as an object
    end
    try
        if(!isopen(RJuliaSocket)) # see comment before parse
            exit()
        end
        JSON.print(RJuliaSocket, toR(value))
        write(RJuliaSocket, "\n")
    catch err
        write(stderr, "Julia Error in returning value of command: ")
        showerror(stderr, err)
        global nInternErrors += 1
        if(nInternErrors > 5)
            write(stderr, "Too many internal errors: exiting\n")
            exit()
        end
        write(RJuliaSocket, "\n") # try to send an empty result back
        continue
    end
end
