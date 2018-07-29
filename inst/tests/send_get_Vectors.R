source(system.file("tests", "mustbe.R", package = "XRJulia"))

ev = XRJulia::RJulia()
set.seed(526)
for( N in c(50,5000)) { # should test both JSON, binary transfer
    cat("Begining test with N =",N, "\n")
    numericTest = rnorm(N)
    integerTest  = as.integer(numericTest*100)
    logicalTest = numericTest > 0
    characterTest = as.character(integerTest)
    complexTest <- complex(numericTest, numericTest*.1)
    rawTest <- charToRaw(paste0(characterTest, collapse = ""))
    length(rawTest) <- N


    mustbe(numericTest, ev$Get(ev$Send(numericTest)))
    mustbe(integerTest, ev$Get(ev$Send(integerTest)))
    mustbe(logicalTest, ev$Get(ev$Send(logicalTest)))
    mustbe(characterTest, ev$Get(ev$Send(characterTest)))
    mustbe(complexTest, ev$Get(ev$Send(complexTest)))
    mustbe(rawTest, ev$Get(ev$Send(rawTest)))
}

