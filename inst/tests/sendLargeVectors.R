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

for(pwr in 3:7) {
  cc =sample(characterTest, as.integer(10^pwr), replace = TRUE)
  cat("pwr = ", pwr, "\n")
  cat("Sending: ", system.time(c2 <- ev$Send(cc)), "\n")
  cat("Getting: ", system.time(c3 <-  ev$Get(c2)), "\n")
}
