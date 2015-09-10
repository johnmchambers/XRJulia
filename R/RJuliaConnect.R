JuliaInterface <- setRefClass("JuliaInterface",
                      fields = c( port = "integer", host = "character",
                          previous = "integer", julia_bin = "character",
                          connection = "ANY", julia_modules = "character"),
                      contains = "Interface")

JuliaInterface$methods(
                       initialize = function(..., startJulia = identical(host, "localhost"), verbose = FALSE){
                           languageName <<- "Julia"
                           prototypeObject <<- JuliaObject()
                           callSuper(...) # set fields
                           args <- list(...)
                           ## start a julia proc.
                           if(length(port) == 0 || is.na(port)) { # uninitialized
                               xport <- getOption("JuliaPort")
                               if(length(xport) == 0 ||is.na(xport))
                                   xport <- basePort + XR::evaluatorNumber(evaluatorId)
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
                                   julia_bin <<- jlFindJulia()
                           }
                           if(verbose)
                               Sys.setenv(JuliaVerbose = 1)
                           else
                               Sys.unsetenv("JuliaVerbose")
                           if(startJulia) {
                               juliaFolder <- system.file("julia", package = .packageName)
                               juliaStart <-  system.file("julia","RJuliaJSON.jl", package = .packageName)
                               Sys.setenv(RJuliaPort=port, RJuliaHost = host, RJuliaSource=juliaFolder)
                               if(host == "localhost")
                                   base::system(paste0(julia_bin, " < ", juliaStart), wait = FALSE)
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
        cmd <- jsonlite::toJSON("quit")
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
        if(is.na(serverPos))
            Call("push!", as.name("LOAD_PATH"), serverDirectory)
        else
            Call("insert!", as.name("LOAD_PATH"), serverPos, serverDirectory)
    },
    Import = function (module = "", ...) {
        members <- unlist(c(...))
        if(length(members)) {
            if(nzchar(module))
                members <- paste(module, members, sep =".")
        }
        else if(nzchar(module))
            members <- module
        else
            return(FALSE)
        imported <- sapply(members, function(x) base::exists(x, envir = modules))
        if (all(imported))
            return(TRUE)
        members <- members[!imported]
        Command(paste("import",paste(members, collapse = ", ")))
        return(members)
    },
    ProxyClassName = function(serverClass) {
        pclass <- callSuper(serverClass)
        if(is.na(pclass) && grepl("{",serverClass, fixed = TRUE)) { # a templated class
            baseClass <- gsub("[{].*","", serverClass)
            callSuper(baseClass) # check for untemplated version
        }
        else
            pclass
    },
    Source = function(filename) {
        Call("include", filename)
    }
)

juliaImport <- function(..., .ev = RJulia())
    .ev$Import(...)

juliaSource <- function(..., .ev = RJulia())
    .ev$Source(filename)

juliaAddToPath <- function(..., .ev = RJulia())
    .ev$AddToPath(filename)

.JuliaInterfaceClass <- getClass("JuliaInterface")

RJulia <- function(...)
    XR::getInterface(.JuliaInterfaceClass, ...)

jlFindJulia <- function() {
    ## See if a location for the Julia executable has been specified
    ## environment variables JULIA_BIN or JULIA_SRC
    envvar <- Sys.getenv("JULIA_BIN")
    if(!nzchar(envvar)) {
        src <- Sys.getenv("JULIA_SRC")
        if(nzchar(src)) {
            ## try either the standard Julia source, or the directory above bin, etc.
            trybin <- paste(src, "usr","bin","julia", sep=.Platform$file.sep)
            if(file.exist(trybin))
                envvar <- trybin
            else {
                trybin <- paste(src, "julia", sep=.Platform$file.sep)
                if(file.exists(trybin))
                    envvar <- trybin
            }
        }
    } # if none of these succeeds, `which julia` needs to return something
    if(!nzchar(envvar)) {
        envvar <- system("which julia", intern = TRUE)
        if(!nzchar(envvar))
            stop("No julia executable in search path and JULIA_BIN environment variable not set")
    }
    envvar
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


JuliaObject <- setRefClass("JuliaObject",
                                contains = "ProxyClassObject")

## use the reshape() function in Julia to create a suitable multi-way array
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
        paste("{", paste(vals, collapse = ", "), "}")
    else {
        onames <- XR::nameQuote(XR::fillNames(onames))
        paste("{", paste(onames, vals, sep = " => ", collapse = ", "),
              "}")
    }
}

setMethod("asServerObject", c("list", "JuliaObject"),
          function(object, prototype)
              .asServerList(object, prototype)
          )

typeToJulia <- function(object, prototype) {
    switch(typeof(object),
           complex = paste0("complex(", typeToJSON(Re(object), prototype, unbox = TRUE), ", ",
           typeToJSON(Im(object), prototype, unbox = TRUE), ")"),
           typeToJSON(object, prototype, unbox = TRUE))
}

## the default method for JuliaObject is modelled on the overall default method in XR
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

## these methods are "copied" from XR only to avoid ambiguity (and a warning message) when selecting
## Otherwise they score the same as the c("ANY", "JuliaObject") methods

for(Class in c("AssignedProxy","ProxyClassObject","ProxyFunction", "name"))
    setMethod("asServerObject", c(Class, "JuliaObject"),
              selectMethod("asServerObject", Class))

juliaSend <- function(object, evaluator = XR::getInterface(.JuliaInterfaceClass))
                          evaluator$Send(object)
juliaGet <- function(object, evaluator = XR::getInterface(.JuliaInterfaceClass))
                          evaluator$Get(object)

## proxy Julia functions
JuliaFunction <- setClass("JuliaFunction",
                          slots = c(module = "character"),
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
setJuliaClass <- function (juliaType, module = "", fields = character(),
    where = topenv(parent.frame()),
    proxyObjectClass = "JuliaObject", ...)
{
    XR::setProxyClass(juliaType, module, fields,
        methods = if(length(fields)) character() else NULL, ServerClass = juliaType,
        where = where, evaluatorClass = "JuliaInterface",
        proxyObjectClass = proxyObjectClass, language = "Julia", ...)
}

## objects converted from Julia
from_Julia <- setClass("from_Julia", contains = "from_Server")

setMethod("initialize", "from_Julia",
          function(.Object, ...) {
              .Object@language <- "Julia"
              .Object@module <- ""
              .Object@Class <- "<UNDEFINED>"
              callNextMethod()
          })

doUnlist <- function(serverClass)
    length(serverClass) == 1 && !grepl("Array.Any,", serverClass) &&
        (grepl("Array.Float", serverClass) || grepl("Array.Int", serverClass) ||
         grepl("Array.[^,]*String", serverClass))

setMethod("asRObject", c("vector_R","JuliaInterface"),
          function (object, evaluator) {
              value <- callNextMethod()
              if(is.list(value) && doUnlist(object@serverClass))
                  unlist(value)
              else
                  value
          })
