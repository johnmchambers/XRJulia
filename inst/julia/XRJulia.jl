module XRJulia

export RJuliaCommand, toR, RObject, vectorR, conditionToR

import JSON


function makeRObject(object::Dict{AbstractString,Any})
    obj = copy(object)
    RClass = pop!(obj, ".RClass") # must be there
    package = pop!(obj, ".package", "")
    Rtype = pop!(obj, ".type", "S4")
    data = pop!(obj, ".Data", nothing)
    delete!(obj, ".extends") # we don't use this
    RObject(RClass, package, Rtype, data, obj)
end

## tables for converting R type/class to Julia: see binaryRVector()
juliaTypes = Dict{String,DataType}( "integer" => Int32, "numeric" => Float64, "character" => String,
                "logical" => Int32, "complex" => Complex{Float64}, "raw" => UInt8, "double" => Float64)

### Converting Array{} types in Julia to basic R vector classes (Not actual typeof())
RTypes = Dict{String, String}("Array{Int32,1}" => "integer", "Array{Float64,1}" => "numeric", "Array{String,1}" => "character",
          "Array{Complex{Float64},1}" => "complex",
          "Array{Bool,1}" => "logical", "Array{Any,1}" => "list", "Array{UInt8,1}" => "raw",
          "Array{Int64,1}" => "integer") 
                              
## the reverse:  making data compatible with R vector types
forRTypes = Dict{String, DataType}("integer" => Array{Int32,1} , "numeric" => Array{Float64,1} , "character" => Array{String,1},
          "complex" =>Array{Complex{Float64},1},
           "logical" =>Array{Bool,1}, "list" => Array{Any,1} )

## the default method:  No idea what to do; presumably junk on the connectio.
function RJuliaCommand(command)
    ctype = string(typeof(command))
    conditionToR("Got a command of unknown type $ctype")
end

function RJuliaCommand(args::Array{Any,1})
    verbose = haskey(ENV, "JuliaVerbose")
    task = args[1]
    if haskey(TaskFunctions, task)
        f = TaskFunctions[task]
        if verbose
            show(string("Doing task: ", args[1], " : ", args[2]))
        end
        ## push the args.  Each task is actually called with a known number
        ## of arguments, but the definitions allow for defaults if those make sense
        ee = Expr(:call)
        aa = Array{Any}(0)
        push!(aa, f)
        for i in 2:length(args)
            push!(aa, args[i])
        end
        ee.args = aa
        eval(ee)
    elseif isa(task, String)
        conditionToR("Not a defined task type: \"$task\"")
    else
        conditionToR(string("Badly formed command:  the 1st element should be a task string, got object of type ", typeof(task)))
    end
end

function RJuliaGet(key::AbstractString)
    try
        eval(parse(key))
    catch err
        conditionToR(string("In Get() with key = \"$key\": ", err))
    end
end

function Rguard(x)
    if isa(x, Tuple)
        [x...]
    else
        x
    end
end


function RJuliaEval(expr::AbstractString, key::AbstractString = "", send = nothing)
    value = nothing; what = "expression"
    try
        if key == ""
            what = "command"
            eval(parse(expr))
            return nothing
        else
            eval(parse(string(key, " = Rguard(", expr, ")")))
            value = eval(parse(key))
        end
    catch err
        msg = string("Evaluating Julia ", what, ": ", JSON.string(expr))
        return conditionToR(msg, err)
    end
    if send == nothing
        objectOrProxy(key, value)
    else
        if send == "TRUE" # the R side sends NA or the character string "TRUE", "FALSE"
            value
        else
            proxyForR(key, value)
        end
    end
end

function RJuliaRemove(key)
end

function RJuliaQuit()
    quit() # that's all follks!
end

## options for Julia.  Anything can be set here via the R juliaOptions() function.  The values below will be
## reset during the initialize() method for the evaluator.
RJuliaParams = Dict{AbstractString, Any}("largeObject" => 1000, "fileBase" => "./juliaToR",
       "fileBaseIndex" => 0)

getVectorFile = function()
    n = RJuliaParams["fileBaseIndex"] + 1
    RJuliaParams["fileBaseIndex"] = n
    string(RJuliaParams["fileBase"],"_",n)
end

function RJuliaSetParams(values::Dict{String, Any})
    for what in keys(values)
        RJuliaParams[what] = values[what]
    end
end

function RJuliaGetParams(what::Array{String, 1})
    value = Dict{String,Any}()
    for key in what
         if haskey(RJuliaParams, key)
            value[key] = RJuliaParams[key]
        else
            value[key] = nothing
        end
    end
    value
end

function RJuliaGetParams(what::String)
    names = Array{String,1}(1)
    names[1] = what
    RJuliaGetParams(names)
end
    
## the names in this table need to match the jlSendTask calls in  RJuliaConnect.R
TaskFunctions = Dict{String, Function}( "get" => RJuliaGet,
                  "eval" => RJuliaEval,
                 "remove" => RJuliaRemove, "quit" => RJuliaQuit,
                   "params" => RJuliaSetParams)
## construct the representation of an R object of class InterfaceError
function conditionToR(msg, err = nothing)
    if err != nothing
        ## would be better to capture the detailed error message somewhere?
        write(STDERR, "Julia error: ")
        showerror(STDERR, err)
        write(STDERR, "\n")
    end
    value = RObject("InterfaceError", "XR")
    value.slots = Dict{AbstractString, Any}("message" => msg) # could one derive expr from err object?
    value
end

RBasic = Union{Number, AbstractString, Bool, Void}
RUnconvertible = Union{DataType, Function}

function treatAsProxy(object)
    true
end

function treatAsProxy(object::RBasic)
    false
end

function objectOrProxy(key, value)
    if treatAsProxy(value)
        proxyForR(key, value)
    else
        value
    end
end

type proxyForR
    key:: AbstractString
    serverClass:: AbstractString
    length:: Int64
end

function proxyForR(key, value)
    len = -1
    try
        len = length(value)
    catch
        len = -1
    end
    proxyForR(key, string(typeof(value)), len)
end

RName = Union{String} # placeholder for future change to AbstractString,or ....
type RObject
    class::RName
    package::RName
    dataType::RName
    data::Any
    slots::Dict{RName, Any}
end

function RObject(class::RName, package::RName = "")
    RObject(class, package, "S4", nothing, Dict{RName, Any}())
end

## the toR() method reverses the interpretation back to a dictionary
## for a specified R class structure
function toR(x::RObject)
    z = copy(x.slots)
    z[".RClass"] = x.class; z[".package"] = x.package; z[".type"] = x.dataType
    if x.data != nothing
        z[".Data"] = x.data
    end
    z
end

function toR(x::proxyForR)
    z = Dict{RName, Any}( ".Data" => x.key )
    z["size"] = x.length; z["serverClass"] = x.serverClass
    z["module"] = "" #? any way to find the module for a type dyamically?
    z[".RClass"] = "AssignedProxy"; z[".package"] = "XR"; z[".type"] = "character"
    z
end

## a general method for toR() constructs an R object of class "from_Julia", the data
## part is a dictionary containing the fields
function toR(x)
    z = Dict{RName, Any}("serverClass" =>  string(typeof(x)))
    d = Dict{RName, Any}()
    nn = fieldnames(x)
    for i in nn
        d[string(i)] = toR(getfield(x, i))
    end
    z["fields"] = d
    obj = RObject("from_Julia", "XRJulia")
    obj.dataType = "S4"
    obj.slots = z
    toR(obj)
end

function toR(x::RUnconvertible)
    z = Dict{RName, Any}("serverClass" => string(typeof(x)),
         "language" => "Julia")
    attr = attributesForR(x)
    if attr != nothing
        z["attributes"] = attr
    end
    obj = RObject("Unconvertible", "XR")
    obj.dataType = "S4"
    obj.slots = z
    toR(obj)
end

toR(x::RBasic) = x

toR(x::Symbol) = string(x)

toR{T}(x::Array{T,1}) = toR(vectorR(x))


function toR(x::Tuple)
    toR([x...])
end

function toR{T,TE}(x::Dict{T,TE})
    xx = copy(x)
    for nn in keys(x)
        xx[nn] = toR(x[nn])
    end
    xx
end

function toR{T,N}(x::Array{T,N})
    dims = size(x)
    ndim = length(dims)
    if ndim == 2
        Class = "matrix"
    else
        Class = "array"
    end
    dim = Array{Int64}(ndim)
    for i in 1:ndim
        dim[i] = dims[i]
    end
    n = prod(dim)
    data = vectorR(reshape(x, n))
    toR(RObject(Class, "methods", data.dataType, toR(data),
                Dict{RName, Any}("dim" => toR(vectorR(dim)))))
end

attributesForR(x) = nothing

attributesForR(x::DataType) = Dict{RName, Any}("typeName" => string(x))


function vectorR(x)
        typeStr = string(typeof(x))
        if haskey(RTypes, typeStr)
            rtype = RTypes[typeStr]
        else
            rtype = "list"
        end
    ## <TODO>  In order to handle Julia's scalar types with no R equivalent, there should be
    ## a mechanism here to set the "type" slot to the actual Julia typeStr, with
    ## a corresponding mechanism in the method for asROject(), ("vector_R", "JuliaInterface")
    mm = Array{Bool}(0) # missing values
    if length(x) > RJuliaParams["largeObject"]
        if(haskey(to_R_direct, rtype))
        method = to_R_direct[rtype]
        return method(rtype, x)
        end
    end
    if rtype == "list"
        value = Array{Any}(size(x))
        for i in 1:length(x)
            value[i] = toR(x[i])
        end
        value
    elseif rtype == "numeric"
        mm = map(isnan, x)
	## uncomment following if we don't count on NaN being passed on by JSON.string()
	## x = copy(x)
        ## if any(mm)
        ##     x[mm] = 0.
        ## end
        value = x
    elseif rtype == "complex" # format this R-style
        fmt(xi) = string(real(xi),"+",imag(xi),"i")
        value = map(fmt, x)
    else
        value = x
    end
    slots = Dict{RName, Any}("data" => value, "type" => rtype,
                                          "missing" => mm)
    RObject("vector_R","XR", "S4", nothing, slots)
end

## functions for direct writing of long vectors
function vector_R_binary(rtype, x)
    if(rtype == "logical" || rtype == "integer") # convert bool or Int64 to Int32
        x = convert(forRTypes["integer"], x)
    elseif rtype == "character"
        function eos(xi::String) xi * "\0" end #need EOS to separate strings
        x = map(eos, x)
    end
    file = getVectorFile()
    write(file, x)
    slots = Dict{RName, Any}("file" => file, "type" => rtype, "length" => length(x))
    RObject("vector_R_direct","XR", "S4", nothing, slots)
end

    # no longer used
# ## for string arrays, Julia writes the strings w/o terminators.
# ## So we return that file plus a second file with the array of string lengths.
# function vector_R_strings(rtype, x)
#     file1= getVectorFile()
#     nbytes = write(file1, x) # the total character count, with no string terminators (? chars, not bytes?)
#     ## a vector of the nchars, compatible with R integer vector
#     lens = convert(forRTypes["integer"], map(length,x))
#     file2 = getVectorFile()
#     write(file2, lens)
#     slots = Dict{RName, Any}("stringFile" => file1, "nbytes" => nbytes, "ncharsFile" => file2, "length" => length(x))
#     RObject("character_R_direct","XR", "S4", nothing, slots)
# end

to_R_direct = Dict{AbstractString, Any}(
    "numeric" => vector_R_binary, "integer" => vector_R_binary,
    "logical" => vector_R_binary, "character" => vector_R_binary)
    


function fieldNames(what::DataType)
    syms = fieldnames(what)
    n = length(syms)
    fields = Array{String}(n)
    for i in 1:n
        fields[i] = string(syms[i])
    end
    fields
end

function classInfo(what::DataType)
    fields = fieldNames(what)
    if what.mutable
        readOnly = nothing
    else
        readOnly = fields
    end
     Dict{RName, Any}("fields" => fields, "readOnly" => readOnly )
end

function binaryRVector(file::AbstractString, vtype::AbstractString, length::Int64)
    if vtype == "character"
        return readRStrings(file, length)
    end
    value = read!(file, Array{juliaTypes[vtype]}(length))
    if vtype == "logical"
        bool = Array{Bool}(length)
        for i in 1:length
            bool[i] = value[i] != 0
        end
        bool
    elseif vtype == "integer"
        convert(Array{Int64,1}, value) # to be consistent with what JSON does
    else
        value
    end
 end

function readRStrings(file::AbstractString, n::Int64)
    text = convert(String, read(file))
    value = convert(Array{String, 1}, split(text, '\0', keep = false))
    if length(value) != n
        write(STDERR, "Warning:  expected to read $n strings with EOS separaters; got $(length(value))")
    end
    value
end

function substrs(text, i, j)
    function sub1(ii, jj) text[ii:jj] end
    map(sub1, i, j)
end

function starts(lens)
    last = 0
    function thisend(len)
       start = last +1
       last = last + len
       start
       end
    map(thisend, lens)
end

function ends(from, lens)
    function plus(i,j) i+j-1 end
    map(plus, from, lens)
end

function makeStringArray(text::String, lens)
    ii = starts(lens)
    jj = ends(ii, lens)
    substrs(text, ii, jj)
end

 

end #module XRJulia
