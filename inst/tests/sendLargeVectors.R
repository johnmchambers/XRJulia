source(system.file("tests", "mustbe.R", package = "XRJulia"))

ev = XRJulia::RJulia()
set.seed(526)
numericTest = rnorm(5000)
integerTest  = as.integer(numericTest*100)
logicalTest = numericTest > 0
characterTest = as.character(integerTest)
mustbe(numericTest, ev$Get(ev$Send(numericTest)))
## the  unlist() should go away when a bug in XR is fixed
mustbe(integerTest, unlist(ev$Get(ev$Send(integerTest))))
mustbe(logicalTest, unlist(ev$Get(ev$Send(logicalTest))))
mustbe(characterTest, unlist(ev$Get(ev$Send(characterTest))))
