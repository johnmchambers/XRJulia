
rjulia_lib = ENV["RJULIA_LIB"]
push!(LOAD_PATH, rjulia_lib)

using XRJulia

import JSON

## start evaluator


verbose = haskey(ENV, "JuliaVerbose")


## all the ENV's should be in a try
RJuliaSource = ENV["RJuliaSource"] # set in R when jlProc() object initialized

RJuliaPort = parse(Int,ENV["RJuliaPort"])
if(verbose)
  show(string("Starting socket on port ",RJuliaPort))
end

RJuliaServer = listen(RJuliaPort)
RJuliaSocket = accept(RJuliaServer)

## the server loop:  reads a JSON object which needs to be an RJulia command
if verbose
    show("Starting parse-eval loop")
end
nInternErrors = 0

while true
    verbose = haskey(ENV, "JuliaVerbose")

    cmd = ["undef", "undef", true, true]
    value = NaN
    try
        if(eof(RJuliaSocket)) # can't help R closing sometimes w/o sending Quit to evaluator
            quit()
        end
        cmd = JSON.parse(RJuliaSocket)
        if verbose
            show(string("JSON parsed cmd: ", cmd))
        end
    catch err
        write(STDERR, "Julia Error/Interrupt in reading command: ")
        showerror(STDERR, err)
        ##### TODO:  distinguish error from interrupt here
        ## nInternErrors += 1
        ## if(nInternErrors > 5)
        ##     write(STDERR, "Too many internal errors: quitting\n")
        ##     quit()
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
            write(STDERR, "Julia Error in evaluating command: ")
            showerror(STDERR, err)
        end
        ## unlike the processing type errors, this is presumably a user error
        ## and is communicated back to R as an object
    end
    try
        if(!isopen(RJuliaSocket)) # see comment before parse
            quit()
        end
        JSON.print(RJuliaSocket, toR(value))
        write(RJuliaSocket, "\n")
    catch err
        write(STDERR, "Julia Error in returning value of command: ")
        showerror(STDERR, err)
        nInternErrors += 1
        if(nInternErrors > 5)
            write(STDERR, "Too many internal errors: quitting\n")
            quit()
        end
        write(RJuliaSocket, "\n") # try to send an empty result back
        continue
    end
end

