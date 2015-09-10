require(XRJulia)
ev <- RJulia()
ev$Eval("1+1")
xx = ev$Eval("%s",1:3)
xx
ev$Get(xx)
ev$Call("length",xx)
xx = ev$Convert(matrix(1:12,3,4))
yy = ev$Get(xx)
str(yy)
ev$Command('Pkg.add("Calculus")')
ev$Command("import Calculus")
ev$Call("Calculus.derivative",as.name("cos"), 1.0)
ev$Call("Calculus.derivative","cos", 1.0) # should be an error
xx = ev$Command("import foobar") # another error
dJ <- JuliaFunction("Calculus.derivative")
dJ(as.name("cos"), 1.0)
cosJ <- JuliaFunction("cos")
dJ(cosJ, 1.0)
cosJ(xx)
yy <- cosJ(xx)
yy
ev$Get(yy)
yy <- cosJ(pi * seq(.25,1.,.25))
pp <- ev$Convert(pi * seq(.25,1.,.25))
dy <- dJ(cosJ, pp) # gets a Julia error; derivative only works on scalars
