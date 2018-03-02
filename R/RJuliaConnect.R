#' An Interface to Julia
#'
#' The JuliaInterface class provides an evaluator for computations in Julia, following the structure
#' in the XR  package.  Proxy functions and classes allow use of the interface with no explicit
#' reference to the evaluator.  The function \code{RJulia()} returns an evaluator object.
#'
#' @field port,host The parameters for communicating with the Julia evaluator.
#' @field julia\_bin The command for starting a Julia process.
#' @field connection The connection object through which commands are sent to Julia
JuliaInterface <- setRefClass("JuliaInterface",
                      fields = c( port = "integer", host = "character",
                          julia_bin = "character",
                          connection = "ANY"),
                      contains = "Interface")

JuliaInterface$methods(
    initialize = function(..., startJulia = identical(host, "localhost"), verbose = FALSE){
        'The initialize method attempts to open a socket unless the "connection" field in the call is an open socket.  Else, if the host is the local host an attempt is made to start a Julia process.  See the documentation of the interface class for details.'
                           languageName <<- "Julia"
                           prototypeObject <<- JuliaObject()
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
                           ## start a julia proc.
                           if(length(port) == 0 || is.na(port)) { # uninitialized
                               xport <- getOption("JuliaPort")
                               if(length(xport) == 0 ||is.na(xport))
                                   xport <- basePort + XR::evaluatorNumber(.self)
                               port <<- as.integer(xport)
                               startJulia <- TRUE
                           }
                           if(startJulia) {
                               ## the equivalent of AddToPath(), done in Julia
                               julia_lib <- system.file("julia",package = "XRJulia")
                               Sys.setenv(RJULIA_LIB = julia_lib)
                               if(!(julia_lib %in% serverPath))
                                   serverPath <<- c(serverPath, julia_lib)
                           }
                           if(length(host) == 0)  ## never set
                               host <<- "localhost"
                           if(identical(host, "localhost")) {
                               if(length(julia_bin) == 0)
                                   julia_bin <<- findJulia()
                           }
                           if(verbose)
                               Sys.setenv(JuliaVerbose = 1)
                           else
                               Sys.unsetenv("JuliaVerbose")
                           if(startJulia) {
                               juliaFolder <- system.file("julia", package = .packageName)
                               juliaStart <-  system.file("julia","RJuliaJSON.jl", package = .packageName)
                               Sys.setenv(RJuliaPort=port, RJuliaHost = host, RJuliaSource=juliaFolder)
                               if(host == "localhost") {
                                   if(!testJSON(julia_bin)) { # try to add the package
                                       jsonAdd <- system.file("julia","addJSON.jl", package = .packageName)
                                       base::system(juliaCMD(julia_bin, jsonAdd))
                                       if(!testJSON(julia_bin))
                                           stop("No JSON module in Julia and unable to add:  try in julia")
                                   }
                                   base::system(juliaCMD(julia_bin, juliaStart), wait = FALSE)
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
                               stop(gettextf("Unable to start Julia connection on port %s: %s",
                                             port, sc$message))
                           connection <<- sc
                           ## now, actions such as setting the path can be performed
                           startupActions()
                       })

## The definition of Julia-dependent methods
JuliaInterface$methods(
    ServerEval = function(expr,key = "", get = NA) {
        value <- ServerTask("eval", expr, key, get)
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
        nzchar(envvar)  && testJSON(envvar) # to protect examples from long delay
    else
        envvar
}

## command to run a julia file.
## Needs to allow for a blank in the Windows location ("Program Files")
juliaCMD <- function(julia_bin, testFile)
    if (.Platform$OS.type == "windows") paste0('"',julia_bin,'" ', testFile) else paste(julia_bin, "<", testFile)

testJSON <- function(julia_bin) {
    testFile <- system.file("julia", "testJSON.jl", package = "XRJulia")
    cmd <-  juliaCMD(julia_bin, testFile)
    hasJSON <- base::system(cmd, intern = TRUE)
    identical("YES", hasJSON)
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
        paste("Dict(", paste(onames, vals, sep = " => ", collapse = ", "),
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
           complex = paste0("complex(", typeToJSON(Re(object), prototype), ", ",
           typeToJSON(Im(object), prototype), ")"),
           typeToJSON(object, prototype))
}

## NOT Roxygen Default Julia method for asServerObject()
## NOT Roxygen
## NOT Roxygen The default method for JuliaObject is modelled on the overall default method in XR.
## NOT Roxygen @param object The \R object.
## NOT Roxygen @param prototype The proxy for a prototype of the Julia object, supplied by the evaluator.
setMethod("asServerObject", c("ANY", "JuliaObject"),
           function(object, prototype) {
               if((isS4(object) && !is.null(attr(object, "class"))) ||
              ## exclude the S4 case where the bit is on to force a JSON array
                  is.list(attributes(object)) || # S3 class or structure
                  is.recursive(object))
                  .asServerList(XR::objectDictionary(object), prototype)
              else #<FIXME> There are some edge cases and NA is not handled well
                  typeToJulia(object, prototype)
           })

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


## these are stubs for a mechanism to convert Julia arrays of non-standard types in R
## See the comments on the toR() methods in XRJulia.jl
doSpecial <- function(typeStr)
    FALSE
doSpecialVector <- function(object)
    NULL

setMethod("asRObject", c("vector_R","JuliaInterface"),
          function (object, evaluator) {
              if(doSpecial(object@type))
                  doSpecialVector(object)
              else
                  callNextMethod()
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
