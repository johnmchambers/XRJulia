module XRJulia

export RJuliaCommand, toR, RObject, vectorR, conditionToR

import JSON

## special processing for array objects via JSON
function objectFromJSON(str::String)
    fromJSONObject(JSON.parse(str))
end

## Interpreting the parsed JSON representation

function fromJSONObject(object)
    ## by default, as is (covers scalars, strings)
    object
end

function fromJSONObject(object::Array{Any, 1})
    ## check for a single type of object
    ## TODO:  should also handle the case that all elements can be converted
    ## to one type; e.g., integer & float; numeric & string
    types = Array{Any}(size(object))
    for i in 1:length(object)
        types[i] = typeof(object[i])
    end
    types = unique(types)
    if length(types) == 1
        convert(typeof(Array{types[1]}(1)), object)
    else
        object
    end
end

## JSON dictionaries are used to encode R classes
function fromJSONObject(object::Dict{AbstractString,Any})
    if haskey(object, ".RClass")
        makeRObject(object)
    else
        object
    end
end

function makeRObject(object::Dict{AbstractString,Any})
    obj = copy(object)
    RClass = pop!(obj, ".RClass") # must be there
    package = pop!(obj, ".package", "")
    Rtype = pop!(obj, ".type", "S4")
    data = pop!(obj, ".Data", nothing)
    delete!(obj, ".extends") # we don't use this
    RObject(RClass, package, Rtype, data, obj)
end

## tables for converting R type/class to Julia:  NOT CURRENTLY USED
juliaTypes = Dict{String,DataType}( "integer" => Int64, "numeric" => Float64, "character" => String,
                "logical" => Bool,  "double" => Float64)

juliaArrayTypes = Dict{String,DataType}( "integer" => Array{Int64,1}, "numeric" => Array{Float64,1},
                     "character" => Array{String,1},
                     "logical" => Array{Bool,1}, "double" => Array{Float64,1} )

### Converting Array{} types in Julia to basic R vector classes (Not actual typeof())
RTypes = Dict{String, String}("Array{Int64,1}" => "integer", "Array{Float64,1}" => "numeric", "Array{String,1}" => "character",
          "Array{Complex{Float64},1}" => "complex",
          "Array{Bool,1}" => "logical", "Array{Any,1}" => "list" )

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

## the names in this table need to match the jlSendTask calls in  RJuliaConnect.R
TaskFunctions = Dict{String, Function}( "get" => RJuliaGet,
                  "eval" => RJuliaEval,
                 "remove" => RJuliaRemove, "quit" => RJuliaQuit)

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
end #module XRJulia
