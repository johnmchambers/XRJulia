#' An Interface to Julia
#'
#' The JuliaInterface class provides an evaluator for computations in Julia, following the structure
#' in the XR  package.  Proxy functions and classes allow use of the interface with no explicit
#' reference to the evaluator.  The function \code{RJulia()} returns an evaluator object.
#'
#' @field host The remote host, as a character string.  By default this will be the local host, and initializing the evalutor
#' will set the field to "localhost".
#' @field port The port number for commuicating to Julia from this evalutor.
#' By default, the port is set by adding the evaluator number-1 to a base port number.
#' By default the base port is randomly chosen at package load time (this strategy may change).
#'
#' The port may be controlled in two ways.  If you know a good range or set of ports, it will
#' be preferrable to supply unique port values (integer) in the initialization call.
#' A less direct way is set the R option "JuliaBasePort", which will then be used as the base port.
#' Since evaluator numbers are used to increment the port, the call to \code{\link{options}} should normally come before 
#' initializing the first Julia evaluator.
#' @field julia_bin The location for an executable version of the Julia interpreter.  By default, this assumes there is a file named \code{"julia"}
#' on the command-line  search path.  If Julia is not usable from the command line or if you want to run with a different version, supply
#' the executable file name as this argument.  It is also possible to set the location for all evaluators by setting the shell variable
#' \code{JULIA_BIN} to this location \emph{before} starting R.
#' @field connection The connection object through which commands are sent to Julia.  Normally will be created by the initialization
#' of the evaluator.  Should only be supplied as a currently
#' open socket on which to communicate with the Julia interpreter.
#' @field serverWrapup a vector of actions for the ServerEval to take after evaluation.  Used to clean up after special operations,
#' such as sending large objects to Julia.
#' @field largeObject Vectors with length bigger than this will be handled specially.  See \link{largeVectors}.  Default currently 1000.  To change this, call \code{\link{juliaOptions}()}
#' to set option \code{largeObject}.
#' @field fileBase a pattern for file names that the evaluator will use in Julia for various data transfer
#' and other purposes.  The evaluator appends "_1", "_2", etc.  To change this, call \code{\link{juliaOptions}()}
#' to set option \code{fileBase}.  It is initiaized to an R tempfile with pattern \code{"Julia"}.
JuliaInterface <- setRefClass("JuliaInterface",
                      fields = c( port = "integer", host = "character",
                          julia_bin = "character",
                          connection = "ANY",
                          serverWrapup = "character",
                          contains = "Interface",
                          largeObject = "integer",
                          fileBase = "character"),
                      contains = "Interface")

JuliaInterface$methods(
                   initialize = function(..., startJulia, verbose = FALSE)
                   {
                       'The initialize method attempts to open a socket unless the "connection" field in the evaluator is an open socket.  Else, if the host is the local host an attempt is made to start a Julia process.  See the documentation of the interface class for details.'
                       languageName <<- "Julia"
                       prototypeObject <<- JuliaObject()
                       prototypeObject$.ev <<- .self
                       largeObject <<- 1000L
                       fileBase <<- tempfile(pattern = "Julia")
                       callSuper(...) # initialize object & register it
                       if(is(connection, "connection")) {
                           if(is(connection, "sockconn") && isOpen(connection))
                               return()
                           else {
                               if(!is(connection, "sockconn"))
                                   msg <- gettextf("got class %s",dQuote(class(connection)))
                               else
                                   msg <- "Socket not open"
                               stop(gettextf(
                                   "Argument connection= should be an open socket connection: %s",
                                   msg))
                           }
                       }
                       if(missing(startJulia))
                           startJulia <- length(host) == 0L || identical(host, "localhost")
                       ## start a julia proc.
                       if(startJulia && (length(port) == 0L || is.na(port))) {# uninitialized
                           base <- getOption("JuliaBasePort")
                           if(is.null(base) || is.na(base))
                               base <- basePort # value set at package load time
                           port <<- as.integer(base + XR::evaluatorNumber(.self)) - 1L
                       }
                       if(startJulia) {
                           Sys.setenv(JULIA_JUNK = "This is a silly environment variable\n")  ## just for testing
                           ## the equivalent of AddToPath(), done in Julia
                           julia_lib <- system.file("julia",package = "XRJulia")
                           Sys.setenv(RJULIA_LIB = julia_lib)
                           if(!(julia_lib %in% serverPath))
                               serverPath <<- c(serverPath, julia_lib)
                       }
                       if(length(host) == 0)  ## never set
                           host <<- "localhost"
                       if(identical(host, "localhost") && length(julia_bin) == 0L)
                               julia_bin <<- findJulia()
                       if(verbose)
                           Sys.setenv(JuliaVerbose = 1)
                       else
                           Sys.unsetenv("JuliaVerbose")
                       if(startJulia) {
                           juliaFolder <- system.file("julia", package = .packageName)
                           juliaStart <-  system.file("julia","RJuliaJSON.jl", package = .packageName)
                           Sys.setenv(RJuliaPort=port, RJuliaHost = host, RJuliaSource=juliaFolder)
                           if(identical(host, "localhost")) {
                               testJSON(julia_bin)
                               juliaCMD(julia_bin, juliaStart, wait = FALSE)
                           }
                       }
                       ## else, the Julia process should have been started and have called accept()
                       ## for the chosen port
                       if(verbose)
                           cat(gettextf(
                               "Starting connection to %s on port %d\n", host, port))
                       for(i in 1:300) {
                           ## waiting for Julia proc. to establish connection
                           Sys.sleep(.2)
                           sc <- tryCatch(socketConnection(host = host, port = port, block = TRUE), error = function(e) e,
                                          warning = function(w)w)
                           if(is(sc, "connection"))
                               break
                           if(verbose)
                               cat(if(i==1) "Waiting." else ".")
                       }
                       cat("\n")
                       if(!is(sc, "connection"))
                           stop(gettextf("Unable to start Julia connection on port %s: got the error/warning message \"%s\"",
                                         port, sc$message))
                       connection <<- sc
                       ## now, actions such as setting the path can be performed
                       startupActions()
                       ## set some julia options--either defaults or specified in the constructor call.
                       juliaOptions(largeObject = largeObject, fileBase = fileBase, .ev = .self)
                   })

## The definition of Julia-dependent methods
JuliaInterface$methods(
    ServerEval = function(expr,key = "", get = NA) {
        value <- ServerTask("eval", expr, key, get)
        on.exit(serverWrapup <<- character())
        for(action in serverWrapup)
            base::eval(base::parse(text = action, encoding = "UTF-8"))
        XR::valueFromServer(value, key, get, .self)
    },
    ServerRemove = function(what)
        XR::valueFromServer(ServerTask("remove", what), "", NA, .self),
    ServerClassDef = function(Class, module = "", ...) {
        value <- juliaClassDef(Class, module, ..., .ev = .self)
        value$methods <- character() # don't make any proxy methods
        value
       },
    ServerTask = function(task, expr,  key = "" , get = NA) {
        'Call the task operation in the Julia code for the interface; the arguments must
be the simple strings or logical value expected.'
        cmd <- jsonlite::toJSON(c(task, expr, key , get))
        writeLines(cmd, connection)
        for(try in 1:10) {
            value <- readLines(connection, 1)
            if(length(value) == 0)  # But shouldn't happen?
                Sys.sleep(1)
            else
                break
        }
        value
    },
    ServerQuit = function(...) {
        cmd <- jsonlite::toJSON("quit") # NB: this is the *task* quit in XRJulia, not the Julia quit() function
        writeLines(cmd, connection) # don't expect an answer; the process will exit
        close(connection)
    },
    ProxyClassName = function(serverClass) {
        'Return the name of a proxy class associated with this server class, or NA.
The proxy class name is the serverClass_Julia.  If the serverClass is parametrized,
the unparamaterized version will be a secondary match.'
        pclass <- serverClass
        if(grepl("{",pclass,fixed = TRUE))
            pclass <- c(pclass, gsub("[{].*","", pclass))
        pclass <- paste(pclass, languageName, sep = "_")
        for(pcl in pclass)
            if(!is.null(getClassDef(pcl)))
                return(pcl)
        NA # will be assigned in table to avoid future search
    }
                       )

## Programming methods
JuliaInterface$methods(
    ServerAddToPath = function(serverDirectory, serverPos = NA) {
        'Julia version of the AddToPath method.'
        ## Julia may try to interpret anything with attributes, so strip
        serverDirectory <- as(serverDirectory, "character")
        if(is.na(serverPos))
            Call("push!", as.name("LOAD_PATH"), serverDirectory)
        else
            Call("insert!", as.name("LOAD_PATH"), serverPos, serverDirectory)
    },
    Import = function (module = "", ..., .command = if(identical(members, "*")) "using" else "import") {
        'import the specified Julia module, optionally for the names provided in the "..."  arguments.  May also be used to invoke the "using" command by giving "*" as the names. In this case multiple modules can be imported by supplying a vector of length > 1 for module. All the exported objects in those modules will be available unqualified.'
        members <- unlist(c(...))
        switch(.command,
               using = {
                   if(length(module) > 1)
                       module <- paste(module, collapse = ", ") # allow multiple modules for using
               },
               import = {
                   if(length(members))
                       module <- paste(module, members, sep=".")
               })
        Command(paste(.command, module))
    },
ProxyClassName = function(serverClass) {
    'Find the proxy class name for a Julia composite type.  Uses the templated version if that has been
defined, otherwise the general untemplated one.'
    pclass <- callSuper(serverClass)
    if(is.na(pclass) && grepl("{",serverClass, fixed = TRUE)) { # a templated class
        baseClass <- gsub("[{].*","", serverClass)
        callSuper(baseClass) # check for untemplated version
    }
    else
        pclass
},
Source = function(filename) {
    'Julia version of the $Source() method, using the include command in Julia'
    Call("include", filename)
},
    Using = function(...) {
        'The Julia "using" form of importing.  Arguments are module names.  All the exported
members of these modules will then be available, without prefix.'
        Import(c(...), .command = "using")
    }
)

#' Function Versions of Methods for Julia Interface evaluators.
#'
#' @name functions
#' @param ... arguments to the corresponding method for an evaluator object.
#' @param evaluator The evaluator object to use.  By default, and usually, the current evaluator
#' is used, and one is started if none has been.  But see the note under \code{juliaImport} for the load actions created in special cases.
NULL

#' @describeIn functions
#' evaluate the file of Julia source.
juliaSource <- function(..., evaluator = RJulia())
    evaluator$Source(...)

#' @describeIn functions
#' adds the directory specified to the search path for Julia modules.
#' If called from the source directory of a package during installation, sets up
#' a load action for that package.  If you want to add the path to all
#' evaluators in \emph{this} session, call the function before creating an evaluator.
#' Otherwise, the action applies only to the specified evaluator or,
#' by default, to the current evaluator.
#' @param directory the directory to add, defaults to "julia"
#' @param package,pos arguments to the method, usually omitted.
#' @param where for the load action, omitted if called from a package source file.
#' Otherwise, must be the environment in which a load action can take place.
juliaAddToPath <- function(directory = "julia", package = utils::packageName(topenv(parent.frame())), pos = NA,  evaluator = RJulia(.makeNew = FALSE),
                           where = topenv(parent.frame())) {
    if(is.null(evaluator))
        XR::serverAddToPath("JuliaInterface", directory, package, pos, where = where)
    else
        evaluator$AddToPath(directory, package, pos)
}

#' @describeIn functions
#' the "using" form of Julia imports:  the module is imported with all exports exposed.
#' @param module String identifying a Julia module.
juliaUsing <- function(module, evaluator) {
    if(missing(evaluator))
        juliaImport(module, "*")
    else
        juliaImport(module, "*", evaluator = evaluator)
}

#' @describeIn functions
#' adds the module information specified to the modules imported for future Julia evaluator objects.
#'
#' Add the module to the table of imports for Julia evaluators, and import it to the current evaluator
#' if there is one.
#' If called from the source directory of a package during installation, both \code{juliaImport}
#' and \code{juliaAddToPath()} set up
#' a load action for that package.  The functional versions, not the methods themselves, should
#' be called from package source files to ensure that the load actions are created.
#' Note that calling either function before any evaluator has been generated
#' will install that call as a setup action for all XRJulia evaluators.
juliaImport <- function(...,  evaluator = RJulia(.makeNew = FALSE)) {
    if(is.null(evaluator))
        XR::serverImport("JuliaInterface", ...)
    else
        evaluator$Import(...)
}



.JuliaInterfaceClass <- getClass("JuliaInterface")

#' An Evaluator for the Julia Interface.
#'
#' Returns an evaluator for the Julia interface.  Starts one on the first call, or if arguments are provided;
#' providing argument \code{.makeNew = TRUE} will force a new evaluator.  Otherwise, the current evaluator is
#' returned.
#'
#' @param ... Arguments passed to \code{\link{getInterface}()} but none usually required.
#' See \code{\link{JuliaInterface}} for details of the evaluator.
RJulia <- function(...)
    XR::getInterface(.JuliaInterfaceClass, ...)

#' Find a Julia Executable
#'
#' This function looks for an executable Julia application in the local operating system.  The location can be prespecified by
#' setting environment variable \code{JULIA_BIN}; otherwise, the function looks in various conventional locations
#' and if that doesn't work, runs a shell command to look for \code{julia}.
#' @return The location as a character string, unless \code{test} is \code{TRUE}, in which case success or failure
#' is returned, and the location found (or the empty string) is saved as the environment variable.
#' Note that in this case, \code{FALSE} is returned if the Julia package \code{JSON} has not been added.
#' 
#' If \code{test} is \code{FALSE}, failure to find a Julia
#' in the current system is an error.
#' @param test Should the function test for the existence of the application.  Default \code{FALSE}. Calling with
#' \code{TRUE} is useful to bullet-proof examples or tests for the absence of Julia. If the test search succeeds,
#' the location is saved in environment variable \code{JULIA_BIN}.
#' @section On Mac OS X:
#' Installing Julia in the usual way does not put the command line version in a
#' standard location, but instead in a folder under \code{/Applications}.
#' Assuming one wants to have Julia available from the command line,
#' creating a symbolic link to it in \code{/usr/local/bin} is a standard approach.
#' If the current version of Julia is \code{0.6}:
#'
#' \code{sudo ln -s /Applications/Julia-0.6.app/Contents/Resources/julia/bin/julia /usr/local/bin/julia}
#'
#' If for some reason you did not want this to be available,  set the shell variable
#' \code{JULIA_BIN} to the first file in the command, the one in \code{/Applications}.
findJulia <- function(test = FALSE) {
    ## See if a location for the Julia executable has been specified
    ## environment variables JULIA_BIN or JULIA_SRC
    envvar <- Sys.getenv("JULIA_BIN")
    if(!nzchar(envvar)) {
        src <- Sys.getenv("JULIA_SRC")
        if(nzchar(src)) {
            ## try either the standard Julia source, or the directory above bin, etc.
            trybin <- paste(src, "usr","bin","julia", sep=.Platform$file.sep)
            if(file.exists(trybin))
                envvar <- trybin
            else {
                trybin <- paste(src, "julia", sep=.Platform$file.sep)
                if(file.exists(trybin))
                    envvar <- trybin
            }
        }
    } # if none of these succeeds, `which julia` used to find an executable version
    if(!nzchar(envvar)) {
        command <-if (.Platform$OS.type == "windows") "where" else "which"
        envvar <- tryCatch(system2(command, "julia", stdout = TRUE), warning = function(e) "")
        if(test)
            Sys.setenv(JULIA_BIN = envvar) # so next call finds this immediately
        else if(!nzchar(envvar))
            stop("No julia executable in search path and JULIA_BIN environment variable not set")
    }
    if(test)
        nzchar(envvar)
    else
        envvar
}

## command to run a julia file.
## Needs to allow for a blank in the Windows location ("Program Files")
juliaCMD <- function(julia_bin, testFile, ...)
    if (.Platform$OS.type == "windows"){
      base::system2(command = julia_bin, args = paste0('"', testFile, '"'), ...)
    } else {
      base::system(command = paste(julia_bin, testFile), ...)
    }

testJSON <- function(julia_bin) {
    testFile <- system.file("julia", "testJSON.jl", package = .packageName)
    if(juliaCMD(julia_bin, testFile) > 0) # error exit from command
      stop("Unable to continue:  failed to get JSON.jl - try Pkg.add() from julia directly")
    TRUE
}

testSockets <- function(julia_bin) {
  testFile <- system.file("julia", "testSockets.jl", package = .packageName)
  if(juliaCMD(julia_bin, testFile) > 0) # error exit from command
    stop("Unable to continue:  failed to get Sockets.jl - try Pkg.add() from julia directly")
  TRUE
}
    

## the base for the port used by JuliaInterface objects
## This value should be overwritten by a random choice in the load action
basePort <- 1233L

.setPort <- function(ns) {
    assign("basePort", as.integer(1000+sample(999,1)), envir = ns)
}
setLoadAction(.setPort, "setJuliaPort")


## Julia will have some methods for data conversion via asServerObject
## The evaluator's prototypeObject field will be from class JuliaObject

#' Proxy Objects in R for Julia Objects
#'
#' @name proxyJuliaObjects
NULL

#' Proxy Objects in R for Julia Objects
#'
#' This is a class for all proxy objects from a Julia class with an R proxy class definition.
#' Objects will normally be from a subclass of this class, for the specific Julia class.
#'
#' Proxy objects returned from the Julia interface will be promoted to objects
#' from a specific R proxy class for their Julia class, if such a class has been defined.
JuliaObject <- setRefClass("JuliaObject",
                           contains = "ProxyClassObject")


## NOT Roxygen  Array method for asServerObject()
## NOT Roxygen
## NOT Roxygen use the reshape() function in Julia to create a suitable multi-way array
## NOT Roxygen @param object The \R object.
## NOT Roxygen @param prototype The proxy for a prototype of the Julia object, supplied by the evaluator.
setMethod("asServerObject", c("array", "JuliaObject"),
          function(object, prototype) {
              ## the data part will be a JSON list, forced by the asS4() for length 1
              data <- asServerObject(asS4(as.vector(object)), prototype)
              dims <- paste(dim(object), collapse = ",")
              value <- gettextf("reshape(%s, %s)", data, dims)
              value
          })

.asServerList <- function(object, prototype) {
    if(length(object) == 0)
        return("{ }")
    onames <- names(object)
    vals <- sapply(object, function(x) asServerObject(x, prototype))
    if(is.null(onames))
        paste("[", paste(vals, collapse = ", "), "]")
    else {
        onames <- XR::nameQuote(XR::fillNames(onames))
        paste("Dict{String, Any}(", paste(onames, vals, sep = " => ", collapse = ", "),
              ")")
    }
}

## NOT Roxygen List method for asServerObject()
## NOT Roxygen
## NOT Roxygen For "plain" lists, produces the Julia expression for a list or a dictionary;
## NOT Roxygen with other attributes, uses the .RClass form.
## NOT Roxygen @param object The \R object.
## NOT Roxygen @param prototype The proxy for a prototype of the Julia object, supplied by the evaluator.
setMethod("asServerObject", c("list", "JuliaObject"),
          function(object, prototype){
              attrs <- attributes(object)
              if(is.null(attrs) || identical(names(attrs), "names"))
                  .asServerList(object, prototype)
              else
                  .asServerList(XR::objectDictionary(object), prototype)
          }
          )

typeToJulia <- function(object, prototype) {
    switch(typeof(object),
           ## For complex, Julia requires an explicitly typed array for the real, imaginary parts
           complex = paste0("complex(Array{Float64,1}(", typeToJSON(Re(object), prototype), ") , Array{Float64,1}(",
                            typeToJSON(Im(object), prototype), "))"),
           ## typeToJSON() will create an RData object if there are NA's:  won't parse in Julia
           ## Instead two work arounds but with warning
           integer = , logical = if(any(is.na(object))) {
               warning(gettextf("Julia cannot represent missing values in type %s; converting to double", typeof(object)))
               typeToJSON(as.numeric(object), prototype)
           } else typeToJSON(object, prototype),
           character = { if(any(is.na(object))) {
               warning("Julia cannot represent missing values in strings; converting to \"<NA>\"")
               object[is.na(object)] <- "<NA>"
                         }
           typeToJSON(object, prototype)},
           raw = rawToJulia(object, prototype),
           typeToJSON(object, prototype))
}

notSimpleVector <- function(object)
    (isS4(object) && !is.null(attr(object, "class"))) ||
              ## exclude the S4 case where the bit is on to force a JSON array
                  is.list(attributes(object)) || # S3 class or structure
                  is.recursive(object)

rawToJulia <- function(object, prototype) {
    if(length(object) > 1 || isS4(object))
        paste0("convert(Array{UInt8,1}, ", typeToJSON(as.integer(object), prototype), ")")
    else
        paste0("convert(UInt8, ", as.integer(object), ")")
}

## NOT Roxygen Default Julia method for asServerObject()
## NOT Roxygen
## NOT Roxygen The default method for JuliaObject is modelled on the overall default method in XR.
## NOT Roxygen @param object The \R object.
## NOT Roxygen @param prototype The proxy for a prototype of the Julia object, supplied by the evaluator.
setMethod("asServerObject", c("ANY", "JuliaObject"),
           function(object, prototype) {
               if(notSimpleVector(object))
                  .asServerList(XR::objectDictionary(object), prototype)
              else #<FIXME> There are some edge cases and NA is not handled well
                  typeToJulia(object, prototype)
           })

#' Internal Computations for Large Vectors
#'
#' @name largeVectors
#'
#' @section Sending Large Vectors between R and Julia:
#' Large vectors will be slow to transfer as JSON, and may fail in Julia.  Internal computations have
#' been added to transfer vectors of types real, integer, logical and character by more direct
#' computations when they are large.  The computations and their implementation are
#' described here.
#'
#'
#' R and Julia both have the concept of numeric (floating point) and integer arrays whose elements have a consistent type and both implement
#' these (following Fortran) as contiguous blocks in memory, augmented by length or dimension information.
#' They also both have a mechanism for arrays of character strings, class \code{"character"} in R and array type
#' \code{Array{String, 1}} in Julia.
#' Julia has arrays for boolean data; R stores the corresponding \code{logical} as integers.
#' 
#' JSON has no such concepts, so interface evaluators using the standard JSON form provided by 'XR' must send such data as a JSON list.  This will
#' become inefficient for very large data from these classes.  Users have reported failure by Julia to
#' parse the corresponding JSON.
#'
#' The 'XRJulia' package (as of version 0.7.9) implements special code to send vectors to Julia, by
#' writing an intermediate file that Julia reads.  The actual text sent to Julia is a call to the
#' relevant Julia function.  The code is triggered within the methods for the \code{asServerObject}
#' function, so vectors should be transferred this way whether on their own or as part of a larger structure,
#' such as an array or the column of a data frame.
#'
#' Similarly, large arrays to be retrieved in R by the \code{Get()} method or the optional argument \code{.get = TRUE}
#' will be written to an intermediate file by Julia and read by R.
#'
#' As vectors become large, direct transfer becomes \emph{much} faster.  On a not-very-powerful laptop,
#' vectors of length \code{10^7} transfer in an elapsed time of a few seconds.  Character vectors are slightly
#' slower than numeric, as explained below, but in all cases it would be hard to do much computation with
#' the data that did not swamp the cost of transfer.  That said, as always it's more sensible to transfer
#' data once and then use the corresponding proxy object in later calls.
#' 
#' @section Details:
#' For all vectors, the method uses binary writes and reads, which are defined
#' in both R and Julia.  No special computationss are needed for numeric, integer, complex and raw.
#' For these, the R binary representation corresponds to array types in Julia.
#' The special pseudo-value \code{NA} is defined for vectors in R, but no corresponding concept exists
#' in Julia.  For numeric and complex vectors, the floating-point pattern \code{NaN} is used.
#' For all other vectors, a warning is issued and either a numeric object or a special character string is used instead.
#' 
#' For logicals, the internal representation in R uses integers.
#' The Julia code when data is sent from R casts the integer array to a boolean array.
#' On the return side, the Julia boolean array is converted to integer before writing.
#'
#' Character vectors take a little more work, partly because of a weirdness in binary writes
#' for string arrays in Julia.  Where R character vectors can be written in binary form and then read
#' back in, writing a \code{String} array in Julia omits the end-of-string character,
#' effectively writing a single string, from which the array cannot
#' be recovered.  Communicating the entire vector to Julia requires
#' that the Julia side uses this information to split the single string resulting from the R binary write
#' by matching the end-of-string character explicitly
#' For sending back to R, the Julia code
#' appends an end-of-string character to each string before writing the array to a file.  This produces the
#' R format for a binary read of a character vector.
#'
#' Two fields in the evaluator object control details.
#' A large object is defined as a vector of length greater than the integer field \code{largeObject}.
#' Julia creates intermediate files for sending large arrays to R by appending sequenctial numbers to a
#' character field \code{fileBase}.  By default, \code{largeObject} and  \code{fileBase} is obtained from
#' \code{\link{tempfile}()} with pattern \code{"Julia"}. Note that all the files are removed at the end of
#' the evaluation of the expression sending or getting the relevant objects.
#'
#' Since these fields must be known to the Julia evaluator, they should \emph{not} be set directly---this will
#' have no effect.  Instead call the function \code{\link{juliaOptions}()} with these parameter names.
#'

setClassUnion("simpleVectorJulia", c("numeric", "integer", "logical", "character", "complex", "raw"))

setMethod("asServerObject", c("simpleVectorJulia", "JuliaObject"),
          function(object, prototype) {
              useTypeToJulia <- notSimpleVector(object) # the general rule
              ## the length test:  KLUDGE alert:  Julia v 0.6 has a bug that prints an erroneous warning the first time on complex
              useTypeToJulia <- useTypeToJulia || (if(is.complex(object)) length(object) < 2 else 
                  length(object) < prototype$.ev$largeObject)
               if(useTypeToJulia)
                  callNextMethod()
              else {
                  file <- tempfile("toJulia")
                  on.exit(addServerWrapup(gettextf('base::system2("rm","%s")', file)))
                  writeBin(as.vector(object), file)
                  gettextf('binaryRVector("%s", "%s", %d)', file, typeof(object), length(object))
              }
          })

# no longer used
## setMethod("asServerObject", c("character", "JuliaObject"),
##           function(object, prototype) {
##               n <- length(object)
##               if(n < prototype$.ev$largeObject || notSimpleVector(object) )
##                  return(callNextMethod())
##               file1 <- tempfile("toJulia")
##               file2 <- tempfile("toJulia")
##               on.exit(addServerWrapup(gettextf('base::system2("rm","%s")', paste(file1, file2))))
##               writeChar(as.vector(object), file1, eos = NULL) # write with no trailing \0 bytes
##               writeBin(nchar(object), file2)
##               gettextf("readRStrings(\"%s\", \"%s\", %d)", file1, file2, length(object))
##           })

addServerWrapup <- function(command, .ev = RJulia())
    .ev$serverWrapup <- c(.ev$serverWrapup,command)



## these methods are "copied" from XR only to avoid ambiguity when selecting
## Otherwise they score the same as the c("ANY", "JuliaObject") method
## which would be wrong if selected (and either way, a message is printed).
## TODO:  copy man/asServerObject.Rd documentation from XR ??
.copyFromXR <- c("AssignedProxy","ProxyClassObject","ProxyFunction", "name")
for(Class in .copyFromXR)
    setMethod("asServerObject", c(Class, "JuliaObject"),
              selectMethod("asServerObject", Class))

#' @describeIn functions
#' sends the \code{object} to Julia, converting it via methods for
#' \code{\link[XR]{asServerObject}} and returns a proxy for the converted object.
juliaSend <- function(object, evaluator = XR::getInterface(.JuliaInterfaceClass))
                          evaluator$Send(object)
#' @describeIn functions
#' converts the proxy object that is its argument to an \R{} object.
juliaGet <- function(object, evaluator = XR::getInterface(.JuliaInterfaceClass))
    evaluator$Get(object)

#' @describeIn functions
#' Print an object in Julia.  Either one object or several arguments as would
#' be given to the Eval() method.
juliaPrint <- function(object, ..., evaluator = XRJulia::RJulia()) {
    if(length(list(...)))
       object <- evaluator$Eval(object, ...)
    evaluator$Command("print(%s)",object)
    cat("\n")
}

#' @describeIn functions
#' evaluates the \code{expr} string subsituting the arguments.  See the corresponding evaluator
#' method for details.
#' @param expr A string that should be legal when parsed and evaluated in Julia.
juliaEval <- function(expr, ..., evaluator = XR::getInterface(.JuliaInterfaceClass))
    evaluator$Eval(expr, ...)

#' @describeIn functions
#' evaluates the \code{expr} string subsituting the arguments; used for a command that is not
#' an expression.
juliaCommand <- function(expr, ..., evaluator = XR::getInterface(.JuliaInterfaceClass))
    evaluator$Command(expr, ...)

#' @describeIn functions
#' call the function in Julia, with arguments given; expr is the string name of the function
juliaCall <- function(expr, ..., evaluator = XR::getInterface(.JuliaInterfaceClass))
    evaluator$Call(expr, ...)

#' @describeIn functions
#' serialize the \code{object} in Julia
#' @param file,append,all Arguments to the evalutor's serialize and unserialize methods. See the reference,
#' Chapter 10.
#' @param object A proxy in R for a Julia object.
juliaSerialize <- function(object,  file, append = FALSE, evaluator = XR::getInterface(.JuliaInterfaceClass))
    evaluator$Serialize(object, file, append)

#' @describeIn functions
#' unserialize the file in Julia
juliaUnserialize <- function(file, all = FALSE, evaluator = XR::getInterface(.JuliaInterfaceClass))
    evaluator$Unserialize(file, all)

#' @describeIn functions
#' return the name by which this proxy object was assigned in Julia
juliaName <- function(object)
    XR::proxyName(object)

#' @describeIn functions
#' Import a Julia module or add a directory to the Julia Search Path
#' If called from the source directory of a package during installation, both \code{juliaImport}
#' and \code{juliaAddToPath()} also set up
#' a load action for that package.  The functional versions, not the methods themselves, should
#' be called from package source files to ensure that the load actions are created.
juliaImport <- function( ...,  evaluator) {
    if(missing(evaluator))
        XR::serverImport("JuliaInterface", ...)
    else
        evaluator$Import(...)
}

#' Get and/or Set Internal Option Parameters in the Julia Evaluator
#'
#' The Julia code for an evaluator maintains a dictionary, \code{RJuliaParams}, of named parameters used to control
#' various evaluation details.  These and any other desired options can be queried and/or set by calls to \code{juliaOptions}.
#' 
#' The function behaves essentially like the \code{\link{options}()} function in R itself, returning a list of the current entries
#' corresponding to unnamed character arguments and setting the parameters named to the value in the corresponding named argument
#' to \code{juliaOptions}.
#' If no parameter corresponding to a name has been set, requesting the corresponding returned value is \code{nothing}, \code{NULL} in R.
#' @param ... arguments to the corresponding method for an evaluator object.
#' @param .ev The evaluator object to use.  By default, and usually, the current evaluator.
#' @return A named list of those parameters requested (as unnamed character string arguments).  If none, an empty list.
#' Note that options are always returned converted to R, not as proxyies.
juliaOptions <- function(..., .ev = XRJulia::RJulia()) {
    args <- list(...)
    toSet <- nzchar(allNames(args))
    value <- list()
    if(any(toSet)) {
        setArgs <- args[toSet]
        ## check for some special options and set the corresponding field in the evaluator.
        ## This also has the effect of validating the object's class.
        for(special in c("largeObject", "fileBase"))
            if(!is.null(setArgs[[special]]))
               .ev$field(special, setArgs[[special]])
        .ev$Call("RJuliaSetParams", setArgs)
    }
    if(any(!toSet))
        value <- .ev$Call("RJuliaGetParams", unlist(args[!toSet]), .get = TRUE)
    value
}


## proxy Julia functions
JuliaFunction <- setClass("JuliaFunction",
                          contains = "ProxyFunction")

setMethod("initialize", "JuliaFunction",
     function (.Object, name, module = "", evaluator = RJulia(), ...)
    {
        if(nzchar(module) && !grepl(".", name, fixed = TRUE))
            name <- paste(module, name, sep=".")
        else if(!nzchar(module) && grepl(".", name, fixed = TRUE))
            module <- gsub("[.][^.]*$", "", name)
        value <- callNextMethod(.Object, name, evaluator = evaluator, module = module, ...)
        if(nzchar(module)) {
            ## ensure module is imported
            fn <- value@.Data
            bdy <- body(fn)
            body(fn) <- substitute({.evaluator$Import(MODULE); BDY},
                                   list(MODULE = module, BDY = bdy))
            value@.Data <- fn
        }
        value
    }
)

#' Information about a Julia Class
#'
#' The Julia class definition information is computed, and converted to R.
#' @return the Julia definition of the specified class, optionally from the module.
#' @param Class,module Strings identifying the Julia composite type and optionally, the module containing it.
#' @param ...,.ev Don't supply these, \code{.ev} defaults to the current Julia interface evaluator.
juliaClassDef <- function(Class, module = "", ..., .ev = RJulia()) {
    if(nzchar(module))
        .ev$Import(module, Class)
    .ev$Eval(gettextf("classInfo(%s)", Class), .get = TRUE)
}

#' Define a Proxy Julia Class (Composite Type)
#'
#' Given the name and optionally the module for a Julia composite type, defines an R proxy
#' class with the same fields as the Julia type.  By default, uses metadata from Julia to
#' find the fields.  If the call supplies the desired field names explicitly, metadata is
#' not used.
#' @param juliaType,module Strings identifying the composite type and optionally the module containing it. In normal use,
#' metadata from Julia is used to find the definition of the type.
#' @param fields,where,proxyObjectClass,... Overriding arguments that should not be used by direct calls from package source code.
setJuliaClass <- function (juliaType, module = "", fields = character(),
    where = topenv(parent.frame()),
    proxyObjectClass = "JuliaObject", ...)
{
    XR::setProxyClass(juliaType, module, fields,
        methods = if(length(fields)) character() else NULL, ServerClass = juliaType,
        where = where, evaluatorClass = "JuliaInterface",
        proxyObjectClass = proxyObjectClass, language = "Julia", ...)
}


## a table to convert Julia arrays of non-standard types, not handled by the signature = c("vectorR", "ANY")
## method for asRObject
doSpecialVector <- list( raw = function(object) as.raw(unlist(object@data))) # should check, but these came from UInt8

setMethod("asRObject", c("vector_R","JuliaInterface"),
          function (object, evaluator) {
              if(is.function(doSpecialVector[[object@type]]))
                  doSpecialVector[[object@type]](object)
              else
                  callNextMethod() # signature c("vectorR", "ANY")
          })

#' Class for General Julia Composite Type Objects
#'
#' The Julia side of the interface will return a general object from a composite type as an R
#' object of class "from_Julia.  its Julia fields (converted to R objects) can be accessed by the \code{$}
#' operator.
#'
#' @slot serverClass the Julia type.
#' @slot module the Julia module, or ""
#' @slot fields the converted versioin of the Julia fields; these are accessed by the \code{$} operator.
setClass("from_Julia", contains = "from_Server")

setMethod("show", "from_Julia",
          function(object) {
              cat(gettextf("R conversion of Julia object of composite type %s\n\nJulia fields:\n",
                           dQuote(object@serverClass)))
              methods::show(as.list(object@fields))
          })

setMethod("initialize", "from_Julia",
    function (.Object, ...)
    {
        .Object@language <- "Julia"
        callNextMethod(.Object, ..., referenceClass = TRUE)
    }
)

## a class for returning large vectors in simple form
setClass("vector_R_direct", slots = c(file = "character", type = "character", length = "integer"))

setMethod("asRObject", c("vector_R_direct", "JuliaInterface"),
          function(object, evaluator) {
              on.exit(base::system(paste("rm ",object@file)))
              readBin(object@file, object@type, object@length)
          })

## ## a class for returning long character vectors
## setClass("character_R_direct", slots = c(stringFile = "character", nbytes = "integer", ncharsFile = "character", length = "integer"))

## setMethod("asRObject", c("character_R_direct", "JuliaInterface"),
##           function(object, evaluator) {
##               on.exit(system(paste("rm ",object@stringFile, object@ncharsFile)))
##               string <- readChar(object@stringFile, object@nbytes, TRUE)
##               n <- object@length
##               lens <- readBin(object@ncharsFile, "integer", n)
##               last <- cumsum(lens)
##               first <- c(0L, last[-n])+1
##               substring(string,first,last)
##           })
