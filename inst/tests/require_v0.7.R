# tests for Julia >= 0.7
## produces warnings or errors in Julia >=0.7 with earlier versions of XRJulia
ev = RJulia()
xx = ev$Send(1:3)
xxx = ev$Get(xx)
yy = ev$Send(list("a",2))
yyy = ev$Get(yy) # no warning
bb = 1:10000 # uses binary transfer
bj = ev$Send(bb)
bbb = ev$Get(bj) # no warning
bjs = ev$Send(paste(bb))
bjss = ev$Get(bjs)
bl = runif(10000)> .5
blj = ev$Send(bl)
blr = ev$Get(blj)
dd = list(a=xxx, b= list(c=yyy, d = paste(bbb)))
ddj = ev$Send(dd)
ddd = ev$Get(ddj)
## order of elements not guaranteed preserved in Julia
all.equal(dd$a, ddd$a); all.equal(dd$b, ddd$b)
ee = as.environment(dd)
### test defining a Julia "class", returning with class definition
tf = system.file("tests", "testClass.jl", package = "XRJulia")
ev$Source(tf)
th = ev$Call("testThing")
th # proxy for object of class "Thing"
ev$Get(th)
Thing <-setJuliaClass("Thing")
t1 <- Thing(pi, "pi thing") # Julia constructor args  must be all fields, unnamed
t1
t1$x
