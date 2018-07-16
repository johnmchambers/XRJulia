mustbe <- function(x,y, verbose = identical(getOption("verbose"), TRUE)) {
    if(identical(all.equal(x,y), TRUE))
    {
        if(verbose) message(gettextf("%s and %s agreed",
                                     deparse(substitute(x)),deparse(substitute(y))))
    }
    else
        stop(gettextf("%s and %s differed",
                      deparse(substitute(x)),deparse(substitute(y))))
    invisible("OK")
}
