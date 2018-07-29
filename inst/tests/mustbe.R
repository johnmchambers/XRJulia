mustbe <- function(x,y, verbose = identical(getOption("verbose"), TRUE)) {
    result <- all.equal(x,y)
    if(identical(result, TRUE))
    {
        if(verbose) message(gettextf("%s and %s agreed",
                                     deparse(substitute(x)),deparse(substitute(y))))
    }
    else
        stop(gettextf("%s and %s differed: %s",
                      deparse(substitute(x)),deparse(substitute(y)),
                      paste(result, collapse = "\n")))
    invisible("OK")
}
